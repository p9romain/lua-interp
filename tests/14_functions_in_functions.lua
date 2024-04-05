-- as a consequence of what we have so far, functions can be defined within
-- functions

function foo ()
  function bar (x)
    print(x)
  end
  bar(1)
  bar(2)
end

foo ()

print ("-------------")
-- functions can access variables of enclosing functions scopes, just like they
-- can access globals

function foo (x)
  function bar (y)
    print(x, y)
  end
  bar (2 * x)
end

foo (1)
foo (2)

print ("-------------")
-- modifying variables happens in the scope where they have been introduced

function foo (x)
  function bar (y)
    x = x + 1      -- modifies the 'x' argument to 'foo'
    y = y + 1      -- modifies the 'y' argument to 'bar'
    print(x, y)
  end
  bar (2 * x)
  -- x has been incremented
  bar (2 * x)
end

foo (1)
