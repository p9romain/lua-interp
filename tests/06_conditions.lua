x = 42

if x > 0 then
  print(x)
end

if x <= 0 then
  print("oops")
end

x = -1

if x > 0 then
  print("positive")
else
  print("negative")
end

a = 12
b = 18
op = "*" -- try changing the value to test the different cases below

if op == "+" then
  res = a + b
elseif op == "-" then
  res = a - b
elseif op == "*" then
  res = a * b
else
  print("invalid operation")
end

print(res)
