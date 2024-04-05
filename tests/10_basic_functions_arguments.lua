print("-----")

function foo(a, b)
  print(a + b)
end

x = 1
y = 2
foo(x, y) -- prints 3

print("------------")
-- variables from an outer scope are accessible from
-- within a function (here, global variables)

x = 42
function foo ()
  print (x)
end

foo () -- prints 42
x = 12
foo () -- prints 12

print("------------")
-- function arguments behave as *local variables* that live
-- in the scope of the function.
-- they shadow (i.e. mask) variables of the same name from
-- outer scopes

function foo (a)
  a = a + 1
  print (a)
end

a = 42
foo(12) -- prints 13
print(a) -- prints 42; 'foo' only modified its local variable 'a'

print("------------")

foo(a) -- prints 43
print(a) -- prints 42 (!), 'foo' only modified its local variable named 'a';
         -- the contents of the global variable 'a' are computed *before*
         -- being passed to 'foo'

print("------------")

function bar (a)
  a = a + 1
  b = b + 1
end

a = 0
b = 0
bar (a)
print(a, b) -- prints 0, 1

a = 0
b = 0
bar (b)
print(a, b) -- prints 0, 1 (!)
