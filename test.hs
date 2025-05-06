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





{-- 
Ejercicio 1 (2 puntos)

problema aproboMasDeNMaterias (registro: seq⟨seq⟨Char⟩ x seq⟨Z⟩⟩, alumno:seq⟨Char⟩, n: Z) : Bool {
  requiere: {No hay nombres de alumnos repetidos en registro}
  requiere: {Las notas de registro son todas iguales o mayores a cero y menores o iguales a 10}
  requiere: {n > 0}
  requiere: {El alumno se encuentra en el registro }
  asegura: {res = true <=> el alumno tiene más de n notas de finales mayores o iguales a 4 en el registro}
--} 
registro::[(String,[Int])]
registro = [("dario",[1,2,3,10]),("damian",[9,9,10])]
alumno = ["dario","damian"]



aproboMasDeNMaterias :: [(String,[Int])]  -> String  -> Int -> Bool
aproboMasDeNMaterias (x:xs) a n |  n < cuantasMateriasAprobo( notasAlumno (x:xs) a )  = True 
                                    | otherwise = False

cuantasMateriasAprobo ::[Int]  -> Int
cuantasMateriasAprobo [] = 0
cuantasMateriasAprobo (x:xs) | x>= 4 = 1 + cuantasMateriasAprobo xs
                             | otherwise = cuantasMateriasAprobo xs


notasAlumno :: [(String, [Int])] -> String -> [Int]
notasAlumno (x:xs) a = snd(encontrarAlumno (x:xs) a)

encontrarAlumno :: [(String, [Int])] -> String ->(String,[Int])
encontrarAlumno [x] a = x
encontrarAlumno ((x,y):xs) a | a==x = (x,y)
                             | otherwise = encontrarAlumno xs a 




--Ejercicio 2 (2 puntos)

--problema buenosAlumnos (registro: seq⟨seq⟨Char⟩ x seq⟨Z⟩⟩) : seq⟨seq⟨Char⟩⟩ {
  --requiere: {No hay nombres de alumnos repetidos en registro}
  --requiere: {Las notas de registro son todas iguales o mayores a cero y menores o iguales a 10}
 --- asegura: {res es la lista de los nombres de los alumnos que están en registro cuyo promedio de notas es mayor o igual a 8 y no tiene aplazos (notas menores que 4)}
--}
--fromIntegral pasa de int a float



buenosAlumnos::[(String,[Int])]-> [String]
buenosAlumnos [] = []
buenosAlumnos ((x1,x2):xs) | promediodeNotas (notasAlumno ((x1,x2):xs) x1)  >= 8 && notaDesaprobada  (notasAlumno ((x1, x2):xs) x1) == False = [x1] ++ buenosAlumnos xs
                           | otherwise = buenosAlumnos xs

sumarNotas::[Int] -> Int
sumarNotas [] = 0
sumarNotas (x:xs) = x + sumarNotas xs

contarNotas::[Int] -> Int
contarNotas [] = 0
contarNotas (x:xs) = 1 + contarNotas xs

promediodeNotas :: [Int] -> Float
promediodeNotas [] = 0
promediodeNotas (x:xs) =  fromIntegral(sumarNotas (x:xs)) / fromIntegral (contarNotas(x:xs))

notaDesaprobada :: [Int] -> Bool
notaDesaprobada [] = False
notaDesaprobada (x:xs) | x < 4 = True  
                       | otherwise = notaDesaprobada xs
                       



--Ejercicio 3 (2 puntos)

--problema mejorPromedio (registro: seq⟨seq⟨Char⟩ x seq⟨Z⟩⟩) : seq⟨Char⟩ {
  --requiere: {No hay nombres de alumnos repetidos en registro}
  --requiere: {Las notas de registro son todas iguales o mayores a cero y menores o iguales a 10}
  --requiere: {|registro| > 0 }
  --asegura: {res es el nombre del alumno cuyo promedio de notas es el más alto; si hay más de un alumno con el mismo promedio de notas, devuelve el nombre de alumno que aparece primero en registro}
--}

mejorPromedio:: [(String,[Int])]-> String
mejorPromedio [x] = fst x
mejorPromedio ((x,y):(x1,x2):xs) | promediodeNotas (notasAlumno((x, y):(x1, x2):xs) x)   >= promediodeNotas(notasAlumno ((x, y):(x1, x2):xs) x1)  = mejorPromedio((x,y):xs)
                                 | otherwise = mejorPromedio((x1,x2):xs)
