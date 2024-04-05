a = 18

function f()
    local a
    a = 1;
    res = function ()
        a = a + 1;
        return a
    end
    a = a + 1;
    return res
end

f1 = f()
f2 = f()

-- Appeler f deux fois crée deux environnements locaux différents.
print(f1(), f1(), f2(), f1())
-- Et cela ne modifie pas l'environnement global
print(a)


-- On a le même comportement si tout ceci est effectué dans un environnement
-- local. En particulier, la valeur dans l'environnement de la fonction
-- appelante n'est pas modifiée.
function main()
    local a;
    a = 42
    function f()
        local a
        a = 1
        res = function ()
            a = a + 1;
            return a
        end
        a = a + 1;
        return res
    end
    f1 = f()
    f2 = f()
    print(f1(), f1(), f2(), f1())
    print(a)
end

