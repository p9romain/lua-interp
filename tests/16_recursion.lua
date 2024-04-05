function f(x)
  if x > 0 then
    print(x)
    f(x-1)
  end
end

f(10)
