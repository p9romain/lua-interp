x = 0
function f() x = x + 1; return 40+x end

function g(a, b, c)
    print(a, b, c)
end

g(f())

print("---")

g(f(), f(), f(), f(), f())
