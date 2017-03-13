local myname = ...

function parse(text)
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
        return {result=quote(temp.result), rest=temp.rest}
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
    return text:match("^%s*(.*)$")
end

function quote(item)
    return cons(sym("quote"), cons(item, nil))
end

function cons(hd, tl)
    local mem = alloc_ptr(2)
    mem.note = "cons"
    mem[0] = hd
    mem[1] = tl
    return mem
end

interned_symbol = {}

function sym(name)
    local mem = interned_symbol[name]
    if mem then return mem end

    mem = alloc_data(#name + 1)
    mem.note = "symb"
    mem:write_string(name)
    interned_symbol[name] = mem
    return mem
end

function list(item, ...)
    if not item then return raw(0)
    else return cons(item, list(...)) end
end

function eval(expr, env)
    if type(expr) == "number" then
        return expr
    elseif type(expr) ~= "userdata" then
        error("can't evaluate " .. type(expr))
    end

    if expr.note == "symb" then
        -- look up variable
        local temp = env[expr]
        if temp then return temp
        else error("unbound symbol " .. expr:read_string()) end
    elseif expr.note == "cons" then
        -- special form or function invocation
        local head = expr[0]
        local tail = expr[1]
        if head.note == "symb" then
            -- invoke special form
            --XXX problem: equal blocks aren't the same key to special_form
            -- solution?: index by userdata not lua table
            return special_form[head](tail, env)
        else
            -- evaluate and invoke
            -- TODO
        end
    else
        -- other memory blocks self-evaluate
        return expr
    end
end

-- local newenv, params, body = tail[0], tail[1][0], tail[1][1][0]

special_form = {
    [sym("quote")] = do_quote,
    [sym("if")] = do_if,
    [sym("fn")] = do_fn,
    [sym("mac")] = do_mac,
    [sym("new")] = do_new,
    [sym("set")] = do_set,
}

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
    local newenv = {}
    setmetatable(newenv, {__index=env})
    return list(sym("#<fn>"), newenv, params, body)
end

function do_mac(args, env)
end

function do_new(args, env)
    local key, value = args[0], eval(args[1][0], env)
    env[key] = value
    return value
end

function do_set(args, env)
    local key, value = args[0], eval(args[1][0], env)

    local function lookup(env, key, value)
        if not env then error("unbound variable " .. key:read_string()) end

        if rawget(env, key) then
            env[key] = value
            return value
        else
            local parent = getmetatable(env).__index
            return lookup(parent, key, value)
        end
    end

    return lookup(env, key, value)
end
