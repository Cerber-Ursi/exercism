module Alternative (altAgeOn) where
-- Alternative approach, testing possible performance improvement.
-- It is based on two assumptions:
-- - literal numeric expressions are evaluated at compile-time;
-- - multiplication is cheaper then division.
-- So, we're trying to make all the divisions occur only with literal numbers,
-- so that at runtime the calculation will consist only of two multiplications.
-- Benchmark, however, shows no significant difference.

import SpaceAge

altAgeOn :: Planet -> Float -> Float
altAgeOn planet seconds = seconds * speedOf planet

speedOf :: Planet -> Float
speedOf Earth = 1 / 31557600
speedOf planet = orbitalSpeedOf planet * speedOf Earth

orbitalSpeedOf :: Planet -> Float
orbitalSpeedOf Mercury = 1 / 0.2408467
orbitalSpeedOf Venus = 1 / 0.61519726
orbitalSpeedOf Earth = 1 / 1
orbitalSpeedOf Mars = 1 / 1.8808158
orbitalSpeedOf Jupiter = 1 / 11.862615
orbitalSpeedOf Saturn = 1 / 29.447498
orbitalSpeedOf Uranus = 1 / 84.016846
orbitalSpeedOf Neptune = 1 / 164.79132