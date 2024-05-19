co = coroutine.create(
    function(a)
        print("arg1", a)
    end
)

coroutine.mini_resume(co, "test1")

co = coroutine.create(
    function(a)
        print("arg2", a)
        print("yield:", coroutine.yield("test3"))
    end
)

print("1st", coroutine.mini_resume(co, "test2"))
print("2nd", coroutine.mini_resume(co, "test4"))