-- function foo (x, y, ...) ... end
--
--   is syntactic sugar for
--
-- foo = function (x, y, ...) ... end
--
-- "function (x, y, ...) ... end" is an anonymous function,
-- just like "fun x y -> ..." in ocaml

function foo ()
  print "hello"
end

-- this is a function call as a statement
foo()

-- functions that return nothing return nil
u = foo()
print(u)

function bar ()
  return 42
end

-- here the function call occurs as an expression
-- (which evaluates to 42)
print(bar())
