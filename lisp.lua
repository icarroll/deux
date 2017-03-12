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
        if text:sub(1,1) == ")" then return {result=raw(0), rest=test:sub(2)} end

        local head = parse_cons_item(text)
        if head.result == nil then error("expected item") end

        text = skip_space(head.rest)
        local tail = parse_tail_cons(text)

        text = skip_space(tail.rest)
        if text:sub(1,1) ~= ")" then error("expected )") end

        return {result=cons(head.result, tail.result), rest=text:sub(2)}
    elseif c == ")" then
        return {result=nil, rest=text:sub(2)}
    elseif c == "." then
        return {result=nil, rest=text:sub(2)}
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
        local temp = parse_cons_item(text)
        if temp.result == nil then error("expected item") end
        return temp
    end

    local temp = parse_cons_item(text)
    if temp.result == nil then return temp end

    local tail = parse_tail_cons(skip_space(temp.rest))

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

symbol_table = {}

function sym(name)
    local mem = symbol_table[name]
    if mem then return mem end

    mem = alloc_data(#name + 1)
    mem.note = "symb"
    mem:write_string(name)
    symbol_table[name] = mem
    return mem
end

if myname == "-t" then
    assert(symbol_char("a"))
    assert(not symbol_char("`"))
    print("OK")
end
