module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / yearOn planet

yearOn :: Planet -> Float
yearOn Earth = 31557600
yearOn planet = orbitalPeriodOf planet * yearOn Earth

orbitalPeriodOf :: Planet -> Float
orbitalPeriodOf Mercury = 0.2408467
orbitalPeriodOf Venus = 0.61519726
orbitalPeriodOf Earth = 1
orbitalPeriodOf Mars = 1.8808158
orbitalPeriodOf Jupiter = 11.862615
orbitalPeriodOf Saturn = 29.447498
orbitalPeriodOf Uranus = 84.016846
orbitalPeriodOf Neptune = 164.79132