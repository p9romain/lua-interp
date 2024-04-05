print(false and false)
print(false and true)
print(true and false)
print(true and true)
print(nil and false)
print(false and nil)
print(1 and nil)
print(nil and 1)
print(0 and 2)
print(42 and 0)

print(false or false)
print(false or true)
print(true or false)
print(true or true)
print(nil or false)
print(false or nil)
print(12 or nil)
print(nil or 13)
print(0 or 2)
print(42 or 0)

if false then
    print "false is false"
end

if nil then
    print "nil is false"
end

if 0 then
    print "ok"
else
    print "0 is true"
end

if 1 then
    print "ok"
else
    print "1 is true"
end

if "" then
    print "ok"
else
    print "empty string is true"
end

x = 1
function f () x = x+1; return true end
print(x, f(), x)
print(x, 1 and f(), x)
print(x, 1 or f(), x)
print(x, nil and f(), x)
print(x, false or f(), x)
