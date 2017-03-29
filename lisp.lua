local myname = ...

SYMBOLS = 0
ENVIRONMENT = 1

function parse(text)
    if text:sub(-1,-1) ~= "\n" then text = text .. "\n" end
    text = skip_space(text)
    local temp = parse_cons_item(text)

    text = skip_space(temp.rest)
    if #text ~= 0 then error(string.format("unexpected %s", text:sub(1,1))) end

    return temp.result
end

function parse_cons_item(text)
    if #text == 0 then return {result=nil, rest=text} end

    local c = text:sub(1,1)

    if c:match("%d") then
        local number
        number, text = text:match("^(%d+)(.*)")
        return {result=math.floor(number), rest=text}
    elseif c == "(" then
        text = skip_space(text:sub(2))
        if text:sub(1,1) == ")" then return {result=raw(0), rest=text:sub(2)} end

        local head = parse_cons_item(text)
        if head.result == nil then error("expected item") end

        text = skip_space(head.rest)
        local tail = parse_tail_cons(text)

        text = skip_space(tail.rest)
        if text:sub(1,1) ~= ")" then
            error("expected ) near " .. text:sub(1,10))
        end

        return {result=cons(head.result, tail.result), rest=text:sub(2)}
    elseif c == ")" then
        return {result=nil, rest=text}
    elseif c == "." then
        return {result=nil, rest=text}
    elseif c == "'" then
        text = skip_space(text:sub(2))
        local temp = parse_cons_item(text)
        if temp.result == nil then error("expected quoted item") end
        return {result=list(sym("quote"), temp.result), rest=temp.rest}
    elseif c == "`" then
        text = skip_space(text:sub(2))
        local temp = parse_cons_item(text)
        if temp.result == nil then error("expected quoted item") end
        return {result=list(sym("quasiquote"), temp.result), rest=temp.rest}
    elseif c == "," then
        if text:sub(2,2) == "@" then
            text = skip_space(text:sub(3))
            local temp = parse_cons_item(text)
            if temp.result == nil then error("expected quoted item") end
            return {result=list(sym("unquote-splicing"), temp.result), rest=temp.rest}
        else
            text = skip_space(text:sub(2))
            local temp = parse_cons_item(text)
            if temp.result == nil then error("expected quoted item") end
            return {result=list(sym("unquote"), temp.result), rest=temp.rest}
        end
    else
        local name
        name, text = text:match("^([%w!$%%&*+-./:<=>?@^_~]+)(.*)")
        return {result=sym(name), rest=text}
    end
end

function parse_tail_cons(text)
    if text:sub(1,1) == "." then
        text = skip_space(text:sub(2))
        local item = parse_cons_item(text)
        if item.result == nil then error("expected item") end
        return item
    end

    local head = parse_cons_item(text)
    if head.result == nil then return {result=raw(0), rest=head.rest} end

    local tail = parse_tail_cons(skip_space(head.rest))

    return {result=cons(head.result, tail.result), rest=tail.rest}
end

function skip_space(text)
    text = text:match("^%s*(.*)$")
    if text:sub(1,1) == ";" then
        text = text:match("^;.-\n(.*)$")
        return skip_space(text)
    else return text
    end
end

function cons(hd, tl)
    local mem = alloc_ptr(2)
    mem.note = "cons"
    mem[0] = hd
    mem[1] = tl
    return mem
end

function sym(name)
    function findsym(syms)
        if syms == raw(0) then return nil
        elseif syms[0]:read_string() == name then return syms[0]
        else return findsym(syms[1])
        end
    end

    local temp = findsym(root[SYMBOLS])
    if temp then return temp end

    local newsym = alloc_data(#name + 1)
    newsym.note = "symb"
    newsym:write_string(name)
    root[SYMBOLS] = cons(newsym, root[SYMBOLS])
    return newsym
end

function list(item, ...)
    if not item then return raw(0)
    else return cons(item, list(...))
    end
end

function eval(expr, env)
    -- io.stdout:write("eval: ") ; show(expr)
    if type(expr) == "number" then
        return expr
    elseif type(expr) ~= "userdata" then
        error("can't evaluate " .. type(expr))
    end

    if not getmetatable(expr) then return expr
    elseif expr.note == "symb" then
        -- look up variable
        local temp = lookup(env, expr)
        if temp then return temp
        else error("unbound symbol " .. expr:read_string())
        end
    elseif expr.note == "cons" then
        -- evaluate and invoke special form or function
        local head = expr[0]
        local tail = expr[1]

        local fn = eval(head, env)
        return do_invoke(fn, tail, env)
    else
        -- other memory blocks self-evaluate
        return expr
    end
end

function eval_list(items, env)
    if items == raw(0) then return items end

    if getmetatable(items) ~= nil and items.note == "cons" then
        local head, tail = items[0], items[1]
        return cons(eval(head, env), eval_list(tail, env))
    else
        return eval(items, env)
    end
end

function eval_list_one(items, env)
    if items == raw(0) then return items end

    local head, tail = items[0], items[1]
    local temp = eval(head, env)
    if tail == raw(0) then return temp
    else return eval_list_one(tail, env)
    end
end

function lookup(env, key)
    if env == raw(0) then return nil
    elseif env[0][0] == key then return env[0][1]
    else return lookup(env[1], key)
    end
end

function extend_env(env, formals, actuals)
    if formals == raw(0) then return env
    elseif formals.note == "symb" then
        return cons(cons(formals, actuals), env)
    else
        local fhead, ftail = formals[0], formals[1]
        local ahead, atail = actuals[0], actuals[1]
        local tempenv = extend_env(env, fhead, ahead)
        return extend_env(tempenv, ftail, atail)
    end
end

function do_quote(args, env)
    return args[0]
end

function do_if(args, env)
    if args == raw(0) then return args   -- no clauses, return nil
    elseif args[1] == raw(0) then return eval(args[0], env)   -- else clause
    else
        local temp = eval(args[0], env)   -- eval condition
        if temp == raw(0) then return do_if(args[1][1], env)   -- false
        else return eval(args[1][0], env)   -- true
        end
    end
end

function do_fn(args, env)
    local params = args[0]
    local body = args[1]
    local newenv = cons(cons(raw(0), raw(0)), env)
    return list(sym("#<fn>"), newenv, params, body)
end

function do_mac(args, env)
    local params = args[0]
    local body = args[1]
    local newenv = cons(cons(raw(0), raw(0)), env)
    return list(sym("#<mac>"), newenv, params, body)
end

function do_invoke(fn, actuals, env)
    if fn[0] == sym("#<fn>") then
        local fnenv, formals, body = fn[1][0], fn[1][1][0], fn[1][1][1][0]
        actuals = eval_list(actuals, env)
        newenv = extend_env(fnenv, formals, actuals)
        return eval_list_one(body, newenv)
    elseif fn[0] == sym("#<mac>") then
        local fnenv, formals, body = fn[1][0], fn[1][1][0], fn[1][1][1][0]
        newenv = extend_env(fnenv, formals, actuals)
        local command = eval_list_one(body, newenv)
        return eval(command, env)
    elseif fn[0] == sym("#<special>") then
        local which = fn[1][0]
        if which == sym("quote") then return do_quote(actuals, env)
        elseif which == sym("if") then return do_if(actuals, env)
        elseif which == sym("fn") then return do_fn(actuals, env)
        elseif which == sym("mac") then return do_mac(actuals, env)
        elseif which == sym("new") then return do_new(actuals, env)
        elseif which == sym("set") then return do_set(actuals, env)
        elseif which == sym("builtin") then return do_builtin(actuals, env)
        end
    elseif fn[0] == sym("#<builtin>") then
        local which = fn[1][0]
        actuals = eval_list(actuals, env)
        if which == sym("issym") then
            local item = actuals[0]
            if type(item) == "userdata" and getmetatable(item) ~= nil
                and item.note == "symb" then return sym("t")
            else return raw(0)
            end
        elseif which == sym("iscons") then
            local item = actuals[0]
            if type(item) == "userdata" and getmetatable(item) ~= nil
                and item.note == "cons" then return sym("t")
            else return raw(0)
            end
        elseif which == sym("issame") then
            local item1, item2 = actuals[0], actuals[1][0]
            if item1 == item2 then return sym("t")
            else return raw(0)
            end
        elseif which == sym("plus") then
            return do_plus(actuals)
        elseif which == sym("minus") then
            return do_minus(actuals)
        elseif which == sym("times") then
            return do_times(actuals)
        elseif which == sym("read_sexpr") then
            local line = readline("")
            return parse(line)
        elseif which == sym("write_sexpr") then
            show(actuals[0])
            return raw(0)
        end
    else
        error("bad invoke")
    end
end

function do_plus(items)
    if items == raw(0) then return 0
    else return items[0] + do_plus(items[1])
    end
end

function do_minus(items)
    if items == raw(0) then return 0
    else return items[0] - do_minus(items[1])
    end
end

function do_times(items)
    if items == raw(0) then return 1
    else return items[0] * do_times(items[1])
    end
end

function do_new(args, env)
    local key, value = args[0], eval(args[1][0], env)
    env[1] = cons(cons(key, value), env[1])
    return value
end

function do_set(args, env)
    local key, value = args[0], eval(args[1][0], env)

    local function lookupset(env)
        if env == raw(0) then error("unbound variable " .. key:read_string())
        elseif env[0][0] == key then env[0][1] = value
        else lookupset(env[1])
        end
    end

    lookupset(env)
    return value
end

function do_builtin(args, env)
    local which = args[0]
    return list(sym("#<builtin>"), which)
end

function show(item)
    seen = {}

    function show_one(item)
        if type(item) == "number" then io.stdout:write(item)
        elseif item == raw(0) then io.stdout:write("nil")
        elseif getmetatable(item) == nil then io.stdout:write(tostring(item))
        elseif item.note == "symb" then io.stdout:write(item:read_string())
        elseif item.note == "cons" then
            if getmetatable(item[0]) ~= nil and item[0].note == "cons"
                and item[0][0] == raw(0) then io.stdout:write("[env]")
            elseif seen[item] then io.stdout:write("...")
            else
                seen[item] = true
                io.stdout:write("(")
                show_one(item[0])
                show_rest(item[1])
                io.stdout:write(")")
            end
        else io.stdout:write("[block " .. item.note .. "]")
        end
    end

    function show_rest(items)
        if getmetatable(items) ~= nil and items.note == "cons" then
            io.stdout:write(" ")
            show_one(items[0])
            show_rest(items[1])
        elseif items ~= raw(0) then
            io.stdout:write(" . ")
            show_one(items)
        end
    end

    show_one(item)
    io.stdout:write("\n")
end

lisp = {}

function lisp.repl(env)
    local stdenv
    if not env then
        env = root[ENVIRONMENT]
        stdenv = true
    else stdenv = false
    end

    while true do
        ::start::

        local line = readline("lisp> ")
        if not line then print() ; break end
        local expr
        ok, msg = xpcall(function () expr = parse(line) end, debug.traceback)
        if not ok then
            print("parse error: " .. msg)
            goto start
        end

        if not expr then goto start end

        local result
        ok, msg = xpcall(function () result = eval(expr, env) end, debug.traceback)
        if not ok then
            print("eval error: " .. msg)
            goto start
        end
        show(result)
    end

    if stdenv then root[ENVIRONMENT] = env
    else return env
    end
end

function lisp.load(filename, env)
    if not env then env = root[ENVIRONMENT] end

    local f = io.open(filename, "r")
    local text = f:read("a")
    f:close()

    text = skip_space(text)
    while #text > 0 do
        local temp = parse_cons_item(text)
        if not temp.result then
            error(string.format("unexpected %s", text:sub(1,1)))
        end

        eval(temp.result, env)
        text = skip_space(temp.rest)
    end
end

if root[ENVIRONMENT] == raw(0) then
    root[ENVIRONMENT] = list(
        cons(raw(0), raw(0)),
        cons(sym("quote"), list(sym("#<special>"), sym("quote"))),
        cons(sym("if"), list(sym("#<special>"), sym("if"))),
        cons(sym("fn"), list(sym("#<special>"), sym("fn"))),
        cons(sym("mac"), list(sym("#<special>"), sym("mac"))),
        cons(sym("new"), list(sym("#<special>"), sym("new"))),
        cons(sym("set"), list(sym("#<special>"), sym("set"))),
        cons(sym("builtin"), list(sym("#<special>"), sym("builtin"))))
end

lisp.load("lisp.lisp")
lisp.repl()
