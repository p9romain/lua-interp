print(false and false) -- false
print(false and true) -- false
print(true and false) -- false
print(true and true) -- true
print(nil and false) -- nil
print(false and nil) -- false
print(1 and nil) -- nil
print(nil and 1) -- nil
print(0 and 2) -- 2
print(42 and 0) -- 0
print()
print(false or false) -- false
print(false or true) -- true
print(true or false) -- true
print(true or true) -- true
print(nil or false) -- false
print(false or nil) -- nil
print(12 or nil) -- 12
print(nil or 13) -- 13
print(0 or 2) -- 0
print(42 or 0) -- 42
print()
if false then
  print "false is false"
end

if nil then
  print "nil is false"
end

if 0 then
  print "ok" -- ok
else
  print "0 is true" 
end

if 1 then
  print "ok" -- ok
else
  print "1 is true"
end

if "" then
  print "ok" -- ok
else
  print "empty string is true"
end
print()
x = 1
function f () x = x+1; return true end
print(x, f(), x) -- 1 true 2
print(x, 1 and f(), x) -- 2 true 3
print(x, 1 or f(), x) -- 3 1 3
print(x, nil and f(), x) -- 3 nil 3
print(x, false or f(), x) -- 3 true 4
