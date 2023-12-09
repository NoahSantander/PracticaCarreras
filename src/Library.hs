module Library where
import PdePreludat

-- Defino mis alias
type Color = String
type Velocidad = Number
type Distancia = Number
type Carrera = [Auto]
type CondicionContraAuto = Auto -> Auto -> Bool
type CondicionContraCarrera = Auto -> Carrera -> Bool
type Puesto = Number
type Tiempo = Number
type Modificador = Number -> Number

-- Defino mis tipos
data Auto = UnAuto {
    color :: Color,
    velocidad :: Velocidad,
    distancia :: Distancia
} deriving Show

-- Inicializo algunos ejemplos
autoAmarillo = UnAuto {color = "Amarillo", velocidad = 10, distancia = 30}
autoRojo = UnAuto {color = "Rojo", velocidad = 10, distancia = 300}
autoAzul = UnAuto {color = "Azul", velocidad = 100, distancia = 30000}
carrera = [autoAmarillo, autoRojo, autoAzul]

-- Defino las funciones
sonDistintos :: CondicionContraAuto
sonDistintos auto1 auto2 = color auto1 /= color auto2 
pocaDistancia :: CondicionContraAuto
pocaDistancia auto1 auto2 = ((< 10).abs) (distancia auto1 - distancia auto2)

estaCerca :: CondicionContraAuto
estaCerca auto1 auto2 = (sonDistintos auto1 auto2) && (pocaDistancia auto1 auto2)
--
algunoEstaCerca :: CondicionContraCarrera
algunoEstaCerca auto carrera = any (estaCerca auto) carrera
leGana :: CondicionContraAuto
leGana auto1 auto2 = distancia auto1 > distancia auto2
lesGanaATodos :: CondicionContraCarrera
lesGanaATodos auto carrera = all (leGana auto) carrera

vaTranquilo :: CondicionContraCarrera
vaTranquilo auto carrera =  (not.algunoEstaCerca auto) carrera && lesGanaATodos auto carrera
--
calcularPuesto :: Auto -> Carrera -> Puesto
calcularPuesto auto carrera = (length(filter (not.leGana auto) carrera))

puesto :: Auto -> Carrera -> Puesto
puesto auto carrera = calcularPuesto auto carrera

-- Defino que un auto corra durante un tiempo
calcularDistancia :: Auto -> Tiempo -> Distancia
calcularDistancia = (*).velocidad

alterarVelocidad :: Modificador
alterarVelocidad = (* 4)
bajarVelocidad :: Velocidad -> Modificador
bajarVelocidad velocidad velocidadABajar 
    | velocidad < velocidadABajar = velocidadABajar - velocidad
    | otherwise = 0

autoCorrer :: Auto -> Tiempo -> Auto
autoCorrer auto tiempo = auto {distancia = distancia auto + calcularDistancia auto tiempo}
