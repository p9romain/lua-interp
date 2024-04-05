t = {} -- une table vide
-- faire print(t) n'a pas beaucoup d'intérêt, ça affiche l'adresse
-- de la table en mémoire (non prédictible)

print(t[1]) -- renvoie nil
t[1] = "abc"
print(t[1])
print(t[0]) -- renvoie nil


-- on autorise aussi les chaînes comme clefs pour les tables
u = {}
u["a"] = 42
u["b"] = 18
print(u["a"], u["b"])

-- la syntaxe foo.x est du sucre syntaxique pour foo["x"]
print(u.a, u.b)

-- on peut aussi construire une table en indiquant des paires
-- clef-valeur.
-- La syntaxe est de la forme { [k1] = v1; [k2] = v2; ... }
-- et { k = v } est du sucre syntaxique pour { ["k"] = v }
u = { ["a"] = 42; ["b"] = 18; [0] = 1 }
print(u.a, u.b, u[0])

u = { a = 42; b = 18; [0] = 1 }
print(u.a, u.b, u[0])
