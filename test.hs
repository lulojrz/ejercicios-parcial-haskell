--CONSIGNA 1
--problema votosEnBlanco (formulas: seq⟨String x String⟩,votos:seq< Z >, cantTotalVotos: Z) : Z {
 -- requiere: {formulasValidas(formulas)}
  --requiere: {|formulas| = |votos|}
 -- requiere: {Todos los elementos de votos son mayores o iguales a 0}
 -- requiere: {La suma de todos los elementos de votos es menor o igual a cantTotalVotos}
 -- asegura: {res es la cantidad de votos emitidos que no correspondieron a niguna de las fórmulas que se presentaron}
--}
formulas :: [(String,String)]
formulas = [("Milei","Marra"),("Kondenada","Kicillof"),("Messi","De Paul")]
votos::[Int]
votos = [1,1,1,1,1,3]
votosTotales::Int
votosTotales = 10

computo:: [Int] -> Int 
computo [] = 0
computo(x:xs) = 1 + computo xs


votosEnBlanco:: [(String,String)] -> [Int] -> Int -> Int
votosEnBlanco _ votosAfirmativos votosTotales = votosTotales - computo  votosAfirmativos


--2) Formulas Válidas [3 puntos]

--problema formulasValidas (formulas: seq⟨String x String⟩) : Bool {
  --requiere: {True}
  --asegura: {(res = true) <=> formulas no contiene nombres repetidos, es decir que cada candidato está en una única fórmula (no se puede ser candidato a presidente y a vicepresidente ni en la misma fórmula ni en fórmulas distintas)}
--}

formulasValidas :: [(String, String)] -> Bool
formulasValidas [] = False
formulasValidas ((x,y):xs) | x==y = False
                           | candidatoPertenece x xs = False
                           | candidatoPertenece y xs = False
                           | otherwise = True


candidatoPertenece:: String  -> [(String, String)] -> Bool
candidatoPertenece _ [] = False
candidatoPertenece n ((a,b):xs)  | n == a = True
                                 | n == b = True
                                 |  otherwise = candidatoPertenece n xs 


