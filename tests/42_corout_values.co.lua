co = coroutine.create (function (a)
    local c
    c = coroutine.yield(a + 2)
    return c * 2
end)

b = coroutine.mini_resume(co, 20)
print(b)
d = coroutine.mini_resume(co, b + 1)
print(d)
print(coroutine.status(co))
