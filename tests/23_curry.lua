function f(a)
    return function(b, c)
        return ((a + 2 + b)*c)
    end
end

print(f(12)(42,13))
