function iter_merge (it1, it2)
  return
    coroutine.create(function ()
        local v1, v2
        v1 = coroutine.mini_resume(it1)
        v2 = coroutine.mini_resume(it2)
        while
          coroutine.status(it1) == "suspended" and
          coroutine.status(it2) == "suspended"
        do
          coroutine.yield({ [1] = v1; [2] = v2 })
          v1 = coroutine.mini_resume(it1)
          v2 = coroutine.mini_resume(it2)
        end
        return { [1] = v1; [2] = v2 }
    end)
end

function iter_range(from, to)
  return
    coroutine.create(function ()
        local incr, nitems
        if from <= to then
          incr = 1
          nitems = to - from
        else
          incr = -1
          nitems = from - to
        end

        while nitems > 0 do
          coroutine.yield(from)
          from = from + incr
          nitems = nitems - 1
        end
        return to
    end)
end

function print_iterator(it)
  while coroutine.status(it) == "suspended" do
    print(coroutine.mini_resume(it))
  end
end

function print_pair_iterator(it)
  local p
  while coroutine.status(it) == "suspended" do
    p = coroutine.mini_resume(it)
    print(p[1], p[2])
  end
end

print("--- 0..10 ---")
print_iterator(iter_range(0, 10))

print("--- 10..0 ---")
print_iterator(iter_range(10, 0))

print("--- 0..10, 10..0 interleaved ---")
print_pair_iterator(iter_merge(iter_range(0, 10), iter_range(10, 0)))
