t = { [1]= 2, [5] = 42, [0] = 98, a = "a" }

i = 0
while i < 10 do
  print(t[1])
  i = i+1
end

t.c = "c"
print(t.a, t.b, t.c)

