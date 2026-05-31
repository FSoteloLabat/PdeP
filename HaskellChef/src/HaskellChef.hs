module HaskellChef where
import PdePreludat


data Componente = UnComponente {
    nombre :: String,
    pesoEnGramos :: Number
}deriving (Show, Eq)

data Plato = UnPlato {
    plato :: String,
    dificultad :: Number,
    componentes :: [Componente]
}deriving (Show, Eq)

data Participante = UnParticipante{
    participante :: String,
    trucosDeCocina :: [TrucoCocina],
    platoEspecialidad :: Plato
}deriving (Show)

type TrucoCocina = Plato -> Plato


--Trucos
agregarComponente :: String -> Number -> TrucoCocina
agregarComponente unIngrediente unaCantidad unPlato = unPlato {componentes = (UnComponente unIngrediente unaCantidad) : componentes unPlato}

endulzar :: Number -> TrucoCocina 
endulzar unaCant unPlato = agregarComponente "Azucar" unaCant unPlato 

salar :: Number -> TrucoCocina
salar unaCant unPlato = agregarComponente "Sal" unaCant unPlato

darSabor :: Number -> Number -> TrucoCocina
darSabor cantSal cantAzucar unPlato = salar cantSal . endulzar cantAzucar $ unPlato

duplicarGramos :: Componente -> Componente 
duplicarGramos unComponente = unComponente {pesoEnGramos = 2 * pesoEnGramos unComponente }

duplicarPorcion :: TrucoCocina
duplicarPorcion unPlato = unPlato {componentes = map duplicarGramos (componentes  unPlato)}

esUnBardo :: Plato -> Bool 
esUnBardo unPlato = dificultad unPlato > 7 && (length . componentes $ unPlato) > 5

modificarDificultad :: Number -> TrucoCocina 
modificarDificultad nuevaDificultad unPlato = unPlato{ dificultad = max 0 . min 10 $ nuevaDificultad }

-- (No me parecen realmente necesarias pero para simplificar codigo esta bien implementarlas
menosGramosQue :: Number -> Componente -> Bool 
menosGramosQue unaCantidad unComponente = pesoEnGramos unComponente < unaCantidad

quitarMenoresA10Gramos :: TrucoCocina
quitarMenoresA10Gramos unPlato = unPlato {componentes = filter (menosGramosQue 10) . componentes $ unPlato}

--)

simplificar :: TrucoCocina 
simplificar unPlato 
    | esUnBardo unPlato = modificarDificultad 5 . quitarMenoresA10Gramos $ unPlato
    | otherwise = unPlato


--Tipos de platos 

tieneComponente :: String -> Plato -> Bool
tieneComponente unIngrediente unPlato = any ((== unIngrediente).nombre) . componentes $ unPlato  

tieneAnimal :: Plato -> Bool 
tieneAnimal unPlato = tieneComponente "Carne" unPlato || tieneComponente "Huevos" unPlato|| tieneComponente "Queso" unPlato  || tieneComponente "Manteca" unPlato ||
                      tieneComponente "Leche" unPlato || tieneComponente "Pollo" unPlato || tieneComponente "Pescado" unPlato 


esVegano :: Plato -> Bool
esVegano unPlato = not (tieneAnimal unPlato)


esSinTACC :: Plato -> Bool
esSinTACC unPlato = not ( tieneComponente "Harina" unPlato)

--EsComplejo es literalmente igual a esBardo asi que no la hago (seria repeticion de logica)

esHipertenso :: Componente -> Bool 
esHipertenso unComponente = nombre unComponente == "Sal" && pesoEnGramos unComponente > 2

noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = any esHipertenso.componentes $ unPlato


--Parte B

salcomp :: Componente
salcomp = UnComponente "sal" 6
pimientacomp :: Componente
pimientacomp = UnComponente "pimienta"  4
carnecomp :: Componente
carnecomp = UnComponente "carne" 50
cebollacomp :: Componente
cebollacomp = UnComponente "cebolla" 10
morroncomp :: Componente
morroncomp = UnComponente "morron" 5
arrozcomp :: Componente 
arrozcomp = UnComponente "arroz" 55


platoPepe :: Plato
platoPepe = UnPlato "Plato Pepe" 8 [salcomp, pimientacomp, carnecomp, cebollacomp, morroncomp, arrozcomp]

pepeRonccino :: Participante
pepeRonccino = UnParticipante "Pepe Ronccino" [darSabor 2 5, simplificar, duplicarPorcion] platoPepe

aplicarTruco:: Plato -> (TrucoCocina) -> Plato
aplicarTruco unPlato unTruco = unTruco unPlato

cocinar :: Participante -> Plato
cocinar unParticipante = foldl (aplicarTruco) (platoEspecialidad unParticipante) (trucosDeCocina unParticipante)


esMasDificil:: Plato -> Plato -> Bool
esMasDificil unPlato otroPlato = (dificultad unPlato) > (dificultad otroPlato)

pesoPlato :: Plato -> Number
pesoPlato unPlato = sum.map pesoEnGramos.componentes $ unPlato

esMasLiviano :: Plato -> Plato -> Bool
esMasLiviano unPlato otroPlato = pesoPlato unPlato < pesoPlato otroPlato

esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = esMasDificil unPlato otroPlato && esMasLiviano unPlato otroPlato

mejorParticipante :: Participante -> Participante -> Participante 
mejorParticipante unParticipante otroParticipante
    | esMejorQue (cocinar unParticipante) (cocinar otroParticipante) = unParticipante
    | otherwise = otroParticipante

type Participantes = [Participante]

participanteEstrella :: Participantes -> Participante 
participanteEstrella [unParticipante] = unParticipante
participanteEstrella (unParticipante : otroParticipante : participantesExtras) = participanteEstrella (mejorParticipante unParticipante otroParticipante : participantesExtras)
