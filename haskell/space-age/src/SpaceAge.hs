module SpaceAge (Planet(..), ageOn, ageOnRecursive) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

-- Canonical solution, hinted to at previous iteration
ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / yearOn planet

yearOn :: Planet -> Float
yearOn planet = orbitalPeriodOf planet * yearOnEarth

-- Original solution, with recursion in yearOn function.
-- Turned out to be about 10% worse (and even worse for the Earth)
ageOnRecursive :: Planet -> Float -> Float
ageOnRecursive planet seconds = seconds / yearOnRecursive planet

yearOnRecursive :: Planet -> Float
yearOnRecursive Earth = yearOnEarth
yearOnRecursive planet = orbitalPeriodOf planet * yearOnRecursive Earth

-- Common constants
yearOnEarth :: Float
yearOnEarth = 31557600

orbitalPeriodOf :: Planet -> Float
orbitalPeriodOf Mercury = 0.2408467
orbitalPeriodOf Venus = 0.61519726
orbitalPeriodOf Earth = 1
orbitalPeriodOf Mars = 1.8808158
orbitalPeriodOf Jupiter = 11.862615
orbitalPeriodOf Saturn = 29.447498
orbitalPeriodOf Uranus = 84.016846
orbitalPeriodOf Neptune = 164.79132