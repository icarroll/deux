local myname = ...

require "mnemonics"

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

do
    --TODO implement symbols as red-black tree
    local function search(root, key)
    end

    function RBTsym(name)
    end
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
        if result ~= raw(0) then show(result) end
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

--[[
-- Populate environment and run interpreter
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
]]

TAG_MASK = 3
PTR_TAG = 0
SYM_TAG = 1
CONS_TAG = 2
INT_TAG = 3

CODE_BLOCK = 0
ICOUNT = 1
DYN_PARENT = 2
STAT_PARENT = 3
DESCRIPTOR = 4
DYN_TREE = 5
LOCALS = 6

function code_writer()
    local cw = {
        labels={},
        stack_next=0,
        stack_max=0,
        locals_count=0,
        localvars_start=LOCALS,
        desc_values={},
        desc_values_start=nil
    }

    function cw:push()
        if self.stack_max < self.stack_next then
            self.stack_max = self.stack_next
        end
        self.stack_next = self.stack_next + 1
        return {"stack", self.stack_next - 1}
    end

    function cw:pop()
        self.stack_next = self.stack_next - 1
        return {"stack", self.stack_next}
    end

    function cw:top()
        return {"stack", self.stack_next - 1}
    end

    function cw:localvar()
        self.locals_count = self.locals_count + 1
        return {"localvar", self.locals_count - 1}
    end

    --TODO deduplicate values
    function cw:desc_value(value)
        table.insert(self.desc_values, value)
        return {"desc_value", #self.desc_values - 1}
    end

    function cw:emit(inst)
        table.insert(self, inst)
    end

    function cw:label(obj)
        self.labels[obj] = #self
    end

    function cw:create_block()
        --inspect = require "inspect" ; print(inspect(self))
        local mem = alloc_code(#self)
        for ix = 1,#self do
            local items = self[ix]
            local op = items[1]
            local raw_args = table.move(items, 2, #items, 1, {})
            args = {}
            for jx = 1,#raw_args do
                local item = raw_args[jx]
                if type(item) == "table" then
                    if self.labels[item] then
                        item = self.labels[item]
                    elseif item[1] == "localvar" or item[1] == "nonlocalvar" then
                        item = item[2] + self.localvars_start
                    elseif item[1] == "desc_value" then
                        item = item[2] + self.desc_values_start
                    elseif item[1] == "stack" then
                        item = item[2] + self.localvars_start
                               + self.locals_count
                    end
                end
                table.insert(args, item)
            end
            --print(op, table.unpack(args))
            mem[ix-1] = calc_func[op](table.unpack(args))
        end
        return mem
    end

    return cw
end

function high14(n)
    return (n >> 16) & 0x3fff
end

function low16(n)
    return n & 0xffff
end

function high16(n)
    return (n >> 16) & 0xffff
end

function make_note(text)
    return string.byte(text:sub(1,1))
           | (string.byte(text:sub(2,2)) << 8)
           | (string.byte(text:sub(3,3)) << 16)
           | (string.byte(text:sub(4,4)) << 24)
end

do
    -- Shared code to create an activation record
    local SUB_AREC = 6   -- Overlaps with LOCALS
    local SUB_AREC_SIZE = 7
    local SUB_CODE_BLOCK = 8
    local TEMP = 9
    local SUB_DESC_SIZE = TEMP+1

    local cw = code_writer()
    cw:emit(ALLOCATE_ALLPTR(SUB_AREC, SUB_AREC_SIZE))
    local note = make_note("arec")
    cw:emit(SET_16l_raw(TEMP,  low16(note)))
    cw:emit(SET_16h_raw(TEMP,  high16(note)))
    cw:emit(SET_NOTE(SUB_AREC, TEMP))
    cw:emit(WRITE_FAR(SUB_AREC, CODE_BLOCK, SUB_CODE_BLOCK))
    cw:emit(WRITE_FAR_imm8(SUB_AREC, ICOUNT, 0))
    cw:emit(GET_LINK_PTR_FAR(SUB_AREC, DYN_PARENT))
    cw:emit(WRITE_FAR(SUB_AREC, STAT_PARENT, STAT_PARENT))
    cw:emit(GET_AREC_FAR(SUB_AREC, DESCRIPTOR))
    cw:emit(WRITE_FAR(SUB_AREC, DYN_TREE, 0)) --TODO get from DYN_PARENT
    cw:emit(RESET_JUMP_AREC(SUB_AREC))
    local create_sub_arec = cw:create_block()

    function compile(expr, stat_env, formals)
        local cw = code_writer()
        cw.desc_values_start = SUB_DESC_SIZE

        local symtab = {}
        if stat_env then
            local mt = {}
            function mt.__index(t, ix)
                local temp = stat_env[ix]
                if temp then
                    local kind, ix, count = table.unpack(temp)
                    if kind == "localvar" then
                        return {"nonlocalvar", ix, 1}
                    elseif kind == "nonlocalvar" then
                        return {"nonlocalvar", ix, count+1}
                    else error("bad variable location")
                    end
                else return nil
                end
            end
            setmetatable(symtab, mt)
        end

        if formals and formals ~= raw(0) then
            cw:emit(GET_LINK_DATA(cw:push()))
            emit_code_for_formals(cw, formals, symtab)
            cw:pop()
        end

        emit_code_for(cw, expr, symtab)
        cw:emit(SET_LINK_DATA(cw:pop()))
        cw:emit(JUMP_AREC(DYN_PARENT))
        code_block = cw:create_block()

        local desc_block = alloc_ptr(SUB_DESC_SIZE + #cw.desc_values)
        desc_block.note = "desc"
        desc_block[CODE_BLOCK] = create_sub_arec
        desc_block[ICOUNT] = 0
        desc_block[DYN_PARENT] = raw(0xdeadbeef)   -- Currently unused
        desc_block[STAT_PARENT] = raw(0xdeadbeef)   -- Fixed up by caller
        desc_block[DESCRIPTOR] = raw(0xdeadbeef)   -- Currently unused
        desc_block[DYN_TREE] = raw(0xdeadbeef)   -- Currently unused
        desc_block[SUB_AREC] = raw(0xdeadbeef)   -- Set by create_sub_arec
        desc_block[SUB_AREC_SIZE] = cw.localvars_start + cw.locals_count
                                    + cw.stack_max + 1
        desc_block[SUB_CODE_BLOCK] = code_block

        for ix = 1, #cw.desc_values do
            desc_block[cw.desc_values_start + ix-1] = cw.desc_values[ix]
        end

        return desc_block
    end
end

function emit_code_for(cw, expr, symtab)
    if type(expr) == "number" then
        local ix = cw:push()
        cw:emit(SET_14h(ix, high14(expr)))
        cw:emit(SET_16l(ix, low16(expr)))
    elseif expr == raw(0) then
        cw:emit(CONST_imm16_raw(cw:push(), 0))
    elseif expr.note == "symb" then
        if symtab[expr] then
            local kind, ix, count = table.unpack(symtab[expr])
            if kind == "localvar" then
                cw:emit(COPY(cw:push(), symtab[expr]))
            elseif kind == "nonlocalvar" then
                cw:emit(COPY(cw:push(), STAT_PARENT))
                for n = 1, count-1 do
                    cw:emit(READ_FAR(cw:top(), cw:top(), STAT_PARENT))
                end
                cw:emit(READ_FAR(cw:top(), cw:top(), symtab[expr]))
            else error("bad variable location")
            end
        else show(expr) ; error("undefined variable")
        end
    elseif expr.note == "cons" then
        if expr[0] == sym("if") then
            local end_if = {}
            emit_code_for_if(cw, expr[1], symtab, end_if)
            cw:label(end_if)
        elseif expr[0] == sym("quote") then
            local ix = cw:desc_value(expr[1][0])
            cw:emit(READ_FAR(cw:push(), DESCRIPTOR, ix))
        elseif expr[0] == sym("cons") then
            emit_code_for_cons(cw)
            local cons_ix = cw:top()
            emit_code_for(cw, expr[1][0], symtab)
            cw:emit(WRITE_FAR(cons_ix, 0, cw:pop()))
            emit_code_for(cw, expr[1][1][0], symtab)
            cw:emit(WRITE_FAR(cons_ix, 1, cw:pop()))
        elseif expr[0] == sym("head") then
            emit_code_for(cw, expr[1][0], symtab)
            cw:emit(READ_FAR(cw:top(), cw:top(), 0))
        elseif expr[0] == sym("tail") then
            emit_code_for(cw, expr[1][0], symtab)
            cw:emit(READ_FAR(cw:top(), cw:top(), 1))
        elseif expr[0] == sym("do") then
            emit_code_for_do(cw, expr[1], symtab)
        elseif expr[0] == sym("mac") then
            local desc = compile(expr[1][1][1][0], symtab, expr[1][1][0])
            symtab[expr[1][0]] = {"macro", desc}
            emit_code_for(cw, raw(0), symtab)
        elseif expr[0] == sym("fn") then
            local desc_template = compile(expr[1][1][0], symtab, expr[1][0])
            local ix = cw:desc_value(desc_template)
            cw:emit(READ_FAR(cw:push(), DESCRIPTOR, ix))
            cw:emit(COPY_BLOCK(cw:top(), cw:top()))
            cw:emit(GET_AREC_FAR(cw:top(), STAT_PARENT))
        elseif expr[0] == sym("new") then
            emit_code_for(cw, expr[1][1][0], symtab)
            local ix = cw:localvar()
            symtab[expr[1][0]] = ix
            cw:emit(COPY(ix, cw:top()))
        elseif expr[0] == sym("set") then
            emit_code_for(cw, expr[1][1][0], symtab)
            local value_ix = cw:top()
            local kind, ix, count = table.unpack(symtab[expr[1][0]])
            if kind == "localvar" then
                cw:emit(COPY(symtab[expr[1][0]], value_ix))
            elseif kind == "nonlocalvar" then
                cw:emit(COPY(cw:push(), STAT_PARENT))
                for n = 1, count-1 do
                    cw:emit(READ_FAR(cw:top(), cw:top(), STAT_PARENT))
                end
                cw:emit(WRITE_FAR(cw:pop(), symtab[expr[1][0]], value_ix))
            else error("bad variable location " .. kind)
            end
        elseif expr[0] == sym("inc") then
            emit_code_for(cw, expr[1][0], symtab)
            cw:emit(ADD_imm8(cw:top(), cw:top(), 1))
        elseif expr[0] == sym("dec") then
            emit_code_for(cw, expr[1][0], symtab)
            cw:emit(SUB_imm8(cw:top(), cw:top(), 1))
        elseif expr[0] == sym("zero?") or expr[0] == sym("not") then
            emit_code_for(cw, expr[1][0], symtab)
            cw:emit(IS_ZERO(cw:top(), cw:top()))
        elseif expr[0] == sym("null?") then
            emit_code_for(cw, expr[1][0], symtab)
            cw:emit(IS_ZERO_raw(cw:top(), cw:top()))
        elseif expr[0] == sym("cons?") then
            emit_code_for(cw, expr[1][0], symtab)
            cw:emit(AND_imm8_raw(cw:top(), cw:top(), TAG_MASK))
            cw:emit(SUB_imm8_raw(cw:top(), cw:top(), CONS_TAG))
            cw:emit(IS_ZERO_raw(cw:top(), cw:top()))
        elseif expr[0] == sym("same?") then
            emit_code_for(cw, expr[1][0], symtab)
            emit_code_for(cw, expr[1][1][0], symtab)
            local ix2 = cw:pop()
            local ix1 = cw:pop()
            cw:emit(SUB_raw(cw:push(), ix1, ix2))
            cw:emit(IS_ZERO_raw(cw:top(), cw:top()))
        elseif expr[0] == sym("list") then
            local items = expr[1]
            if getmetatable(items) and items.note == "cons" then
                emit_code_for_cons(cw)   -- Emit head cons of list
                local cons_ix = cw:top()
                cw:emit(COPY(cw:push(), cons_ix))   -- Copy for building list
                emit_code_for_create_list(cw, items, symtab)
                cw:pop()   -- Pop last cons of list
            else emit_code_for(cw, items, symtab)
            end
        elseif expr[0] == sym("+") then
            emit_code_for(cw, expr[1][0], symtab)
            emit_code_for(cw, expr[1][1][0], symtab)
            local ix2 = cw:pop()
            local ix1 = cw:pop()
            cw:emit(ADD(cw:push(), ix1, ix2))
        elseif expr[0] == sym("-") then
            emit_code_for(cw, expr[1][0], symtab)
            emit_code_for(cw, expr[1][1][0], symtab)
            local ix2 = cw:pop()
            local ix1 = cw:pop()
            cw:emit(SUB(cw:push(), ix1, ix2))
        elseif expr[0] == sym("*") then
            emit_code_for(cw, expr[1][0], symtab)
            emit_code_for(cw, expr[1][1][0], symtab)
            local ix2 = cw:pop()
            local ix1 = cw:pop()
            cw:emit(MUL(cw:push(), ix1, ix2))
        elseif expr[0] == sym("=") then
            emit_code_for(cw, expr[1][0], symtab)
            emit_code_for(cw, expr[1][1][0], symtab)
            local ix2 = cw:pop()
            local ix1 = cw:pop()
            local ix = cw:push()
            cw:emit(SUB(ix2, ix1, ix2))
            cw:emit(IS_ZERO(cw:top(), cw:top()))
        elseif expr[0] == sym(">") then
            emit_code_for(cw, expr[1][0], symtab)
            emit_code_for(cw, expr[1][1][0], symtab)
            local ix2 = cw:pop()
            local ix1 = cw:pop()
            cw:emit(SUB(cw:push(), ix1, ix2))
            cw:emit(RSHIFT_imm8(cw:top(), cw:top(), 29))
            cw:emit(XOR_imm8(cw:top(), cw:top(), 1))
        else
            if (symtab[expr[0]] or {})[1] == "macro" then
                -- Macro expansion
                local mac = symtab[expr[0]][2]
                local result = call_vm(mac, expr[1])
                emit_code_for(cw, result, symtab)
            else
                -- Function application
                emit_code_for(cw, expr[0], symtab)
                local actuals = expr[1]
                if getmetatable(actuals) and actuals.note == "cons" then
                    emit_code_for_cons(cw)   -- Emit head cons of list
                    local cons_ix = cw:top()
                    cw:emit(COPY(cw:push(), cons_ix))   -- Copy for building list
                    emit_code_for_create_list(cw, actuals, symtab)
                    cw:pop()   -- Pop last cons of list
                else emit_code_for(cw, actuals, symtab)
                end
                cw:emit(SET_LINK_DATA(cw:pop()))
                cw:emit(JUMP_AREC(cw:pop()))
                cw:emit(GET_LINK_DATA(cw:push()))
            end
        end
    else error("bad compile")
    end
end

function emit_code_for_if(cw, clauses, symtab, end_if)
    if clauses == raw(0) then
        emit_code_for(cw, clauses, symtab)
    else
        emit_code_for(cw, clauses[0], symtab)
        if clauses[1] ~= raw(0) then
            local next_clause = {}
            cw:emit(JUMP_UNLS_imm16(cw:pop(), next_clause))
            emit_code_for(cw, clauses[1][0], symtab)
            cw:emit(JUMP_imm24(end_if))
            cw:label(next_clause)
            cw:pop()
            emit_code_for_if(cw, clauses[1][1], symtab, end_if)
        end
    end
end

function emit_code_for_do(cw, exprs, symtab)
    if exprs == raw(0) then
        emit_code_for(cw, exprs, symtab)
    else
        emit_code_for(cw, exprs[0], symtab)
        if exprs[1] ~= raw(0) then
            cw:pop()
            emit_code_for_do(cw, exprs[1], symtab)
        end
    end
end

function emit_code_for_formals(cw, formals, symtab)
    if formals == raw(0) then return
    elseif formals.note == "symb" then
        local ix = cw:localvar()
        symtab[formals] = ix
        cw:emit(COPY(ix, cw:top()))
    elseif formals.note == "cons" then
        local cons_ix = cw:top()
        local tail_ix = cw:top()
        local head_ix = cw:push()
        cw:emit(READ_FAR(head_ix, cons_ix, 0))
        cw:emit(READ_FAR(tail_ix, cons_ix, 1))
        emit_code_for_formals(cw, formals[0], symtab)
        cw:pop()
        emit_code_for_formals(cw, formals[1], symtab)
    else error("bad formals")
    end
end

function emit_code_for_create_list(cw, actuals, symtab)
    local cons_ix = cw:top()
    emit_code_for(cw, actuals[0], symtab)
    cw:emit(WRITE_FAR(cons_ix, 0, cw:pop()))

    if getmetatable(actuals[1]) and actuals[1].note == "cons" then
        emit_code_for_cons(cw)
        cw:emit(WRITE_FAR(cons_ix, 1, cw:top()))
        cw:emit(COPY(cons_ix, cw:pop()))
        emit_code_for_create_list(cw, actuals[1], symtab)
    else
        local cons_ix = cw:top()
        emit_code_for(cw, actuals[1], symtab)
        cw:emit(WRITE_FAR(cons_ix, 1, cw:pop()))
    end
end

function emit_code_for_cons(cw)
    --[[
    cw:emit(ALLOCATE_ALLPTR_imm16(cw:push(), 2))
    local cons_ix = cw:top()
    cw:emit(OR_imm8_raw(cons_ix, cons_ix, CONS_TAG))
    local note = make_note("cons")
    cw:emit(SET_16l_raw(cw:push(),  low16(note)))
    cw:emit(SET_16h_raw(cw:top(),  high16(note)))
    cw:emit(SET_NOTE(cons_ix, cw:pop()))
    ]]
    cw:emit(ALLOCATE_CONS(cw:push()))
end

function call_vm(sub_arec, arg)
    local cblock = alloc_code(2)
    cblock[0] = calc_func.JUMP_AREC(2)
    cblock[1] = calc_func.HALT()

    local dblock = alloc_ptr(3)
    dblock.note = "mstr"
    dblock[CODE_BLOCK] = cblock
    dblock[ICOUNT] = 0
    dblock[2] = sub_arec

    return execute(dblock, arg)
end

function pcomp(text)
    return compile(parse(text))
end

function scall(sub, arg)
    arg = arg or raw(0)
    show(call_vm(sub, arg))
end

function run(text)
    show(call_vm(compile(parse(text)), raw(0)))
end
