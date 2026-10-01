type Name = String
type Fuel = Int
type Consumption = Double
type Distance = Double
type Speed = Double
data ShipClass = Fighter | Cruiser | Freighter | Explorer
    deriving (Show, Eq)

ship :: (Name, Fuel, Consumption, Distance, Speed, ShipClass) 
     -> ((Name, Fuel, Consumption, Distance, Speed, ShipClass) -> t) -> t
ship (name, fuel, consumption, distance, speed, shipClass) = \msg -> 
    msg (name, fuel, consumption, distance, speed, shipClass)

printShipInfo :: (((Name, Fuel, Consumption, Distance, Speed, ShipClass) -> String) -> String) -> String
printShipInfo someShip = 
    someShip (\(name, fuel, consumption, distance, speed, shipClass) ->
        "Name: " ++ name ++ 
        ", Class: " ++ show shipClass ++
        ", Fuel: " ++ show fuel ++ 
        ", Consumption/parsec: " ++ show consumption ++
        ", Distance: " ++ show distance ++ 
        ", Speed: " ++ show speed)

fly :: Distance -> (((Name, Fuel, Consumption, Distance, Speed, ShipClass) -> 
      ((Name, Fuel, Consumption, Distance, Speed, ShipClass) -> t1) -> t1) -> t2) -> t2
fly n someShip = someShip (\(name, fuel, consumption, distance, speed, shipClass) ->
    let maxDistByFuel = fromIntegral fuel / consumption
        actualDist = min n maxDistByFuel
        fuelSpent = actualDist * consumption
    in ship (name, fuel - round fuelSpent, consumption, distance + actualDist, speed, shipClass))

refuel :: (((Name, Fuel, Consumption, Distance, Speed, ShipClass) -> 
           ((Name, Fuel, Consumption, Distance, Speed, ShipClass) -> t1) -> t1) -> t2) -> t2
refuel someShip = someShip (\(name, _, consumption, distance, speed, shipClass) -> 
    ship (name, 1000, consumption, distance, speed, shipClass))

spaceRaceSim :: (((Name, Fuel, Consumption, Distance, Speed, ShipClass) -> 
                  ((Name, Fuel, Consumption, Distance, Speed, ShipClass) -> t1) -> t1) -> t2) -> t2
spaceRaceSim someShip = someShip (\(name, fuel, consumption, distance, speed, shipClass) -> 
    go (name, fuel, consumption, distance, speed, shipClass))
    where 
        go (n, f, c, d, s, cl) 
            | f < round (s * c) = ship (n, 0, c, d, s, cl)
            | otherwise = go (n, f - round (s * c), c, d + s, s, cl)

spaceRace :: [((Name, Fuel, Consumption, Distance, Speed, ShipClass) -> 
                ((Name, Fuel, Consumption, Distance, Speed, ShipClass) -> t1) -> t1) -> 
               ((Name, Fuel, Consumption, Distance, Speed, ShipClass) -> 
                (Name, Fuel, Consumption, Distance, Speed, ShipClass)) -> 
               (Name, Fuel, Consumption, Distance, Speed, ShipClass)] 
          -> ((Name, Fuel, Consumption, Distance, Speed, ShipClass) -> t) -> t
spaceRace ships = 
    let 
        extract shp = shp (\(n, f, c, d, s, cl) -> (n, f, c, d, s, cl))
        afterRace = map spaceRaceSim ships
        foldFunc acc shp = 
            let (_, _, _, distAcc, _, _) = acc
                (_, _, _, distShp, _, _) = shp
            in if distShp > distAcc then shp else acc
        best = foldl1 foldFunc (map extract afterRace)
    in ship best

------

data Spaceship = Spaceship{ shipName :: Name, shipFuel :: Fuel, shipConsumption :: Consumption, shipDistance :: Distance, shipSpeed :: Speed, shipClassType :: ShipClass, maxFuel :: Fuel} 
    deriving (Show)

flySpaceship :: Distance -> Spaceship -> Spaceship
flySpaceship n someShip = someShip { shipFuel = newFuel, shipDistance = shipDistance someShip + actualDist }
    where maxDistByFuel = fromIntegral (shipFuel someShip) / shipConsumption someShip
          actualDist = min n maxDistByFuel
          fuelSpent = actualDist * shipConsumption someShip
          newFuel = shipFuel someShip - round fuelSpent

refuelSpaceship :: Spaceship -> Spaceship
refuelSpaceship someShip = someShip { shipFuel = maxFuel someShip }

spaceRaceSimSpaceship :: Spaceship -> Spaceship
spaceRaceSimSpaceship someShip
    | shipFuel someShip < round (shipSpeed someShip * shipConsumption someShip) = someShip { shipFuel = 0 }
    | otherwise = spaceRaceSimSpaceship someShip { shipFuel = shipFuel someShip - round (shipSpeed someShip * shipConsumption someShip),
        shipDistance = shipDistance someShip + shipSpeed someShip }

spaceRaceSpaceship :: [Spaceship] -> Spaceship
spaceRaceSpaceship ships = foldl1 (\acc shp -> if shipDistance shp > shipDistance acc then shp else acc) afterRace
    where afterRace = map spaceRaceSimSpaceship ships