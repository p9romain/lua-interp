function f ()
  local i; i = 1
  while i <= 10 do
    print(i)
    coroutine.yield()
    i = i + 1
  end
end

co = coroutine.create (f)

coroutine.mini_resume(co)
coroutine.mini_resume(co)
coroutine.mini_resume(co)
coroutine.mini_resume(co)
coroutine.mini_resume(co)
