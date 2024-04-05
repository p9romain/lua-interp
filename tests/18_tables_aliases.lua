-- les tables sont mutables et suivent les mêmes règles que les Hashtbl (ou tout autre structure mutable) en OCaml:
--   si plusieurs morceaux de programme ont accès à la même valeur "table",
--   modifier le contenu d'une table via n'importe quel morceau de programme
--   modifie la table de manière visible par tout les endroits du programme

t = { a = 0; b = 1 }
print(t.a, t.b)
t2 = t

function incra(u)
  u.a = u.a + 1
end

incra(t)
print(t.a, t.b)
incra(t)
print(t.a, t.b)
print(t2.a, t2.b)

t2.b = 42
print(t.a, t.b)
