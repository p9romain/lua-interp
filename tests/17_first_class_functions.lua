-- print is a function, which can be used as a first-class value

x = print
x(12, 21, 31)

function f(p)
  p(42)
end

f(x)

-- functions can be returned and passed as values
-- the same scope rules as before still apply

function foo (x)
  function get_x ()
    return x
  end
  return get_x
end

f = foo(42)
print (f()) -- prints 42

print("---------------")
-- functions capture their scope: they can always access / modify
-- variables they have access to

function counter (x)
  -- 'count' captures the variable 'x' (parameter the function 'counter' that
  -- calls it)
  function count ()
    x = x + 1
    return x
  end
  return count
end

c = counter(0)
print(c()) -- prints 0
print(c()) -- prints 1
print(c()) -- prints 2

c2 = counter(0)
print(c2()) -- prints 0
print(c2()) -- prints 1


print("---------------")
-- and functions can be passed as arguments to other functions
function iterate(f)
  i = 0
  while i < 10 do
    f (i)
    i = i + 1
  end
end

function pr(i)
  print(i)
end

iterate(pr)
-- this is equivalent to:
iterate(function (i) print(i) end)
