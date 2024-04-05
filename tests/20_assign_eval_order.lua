x = {}
x.a = 1
function f() y = 1; return x end

y = 2
f().a = y
print(x.a, y)


x = {}
z = {}
y = 0;

function f() print(x.a); x.a = 1; return x end
function g() print(x.a); x.a = 2; return "a" end
function h() print(x.a); x.a = 3; return 12 end

f()[g()] = 12
print(x.a)

