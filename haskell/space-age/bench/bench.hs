module Main where

import SpaceAge
import Alternative
import Criterion
import Criterion.Main

seconds :: [Float]
seconds = map fromIntegral ([100,200..2000] ++ [3000,4000..50000] ++ [100000,105000..1000000])

main = defaultMain 
    [ 
        bgroup "Mercury" [
            bench "original" $ nf (map (ageOn Mercury)) seconds,
            bench "alternative" $ nf (map (altAgeOn Mercury)) seconds
        ],
        bgroup "Venus" [
            bench "original" $ nf (map (ageOn Venus)) seconds,
            bench "alternative" $ nf (map (altAgeOn Venus)) seconds
        ],
        bgroup "Earth" [
            bench "original" $ nf (map (ageOn Earth)) seconds,
            bench "alternative" $ nf (map (altAgeOn Earth)) seconds
        ],
        bgroup "Mars" [
            bench "original" $ nf (map (ageOn Mars)) seconds,
            bench "alternative" $ nf (map (altAgeOn Mars)) seconds
        ],
        bgroup "Jupiter" [
            bench "original" $ nf (map (ageOn Jupiter)) seconds,
            bench "alternative" $ nf (map (altAgeOn Jupiter)) seconds
        ],
        bgroup "Saturn" [
            bench "original" $ nf (map (ageOn Saturn)) seconds,
            bench "alternative" $ nf (map (altAgeOn Saturn)) seconds
        ],
        bgroup "Uranus" [
            bench "original" $ nf (map (ageOn Uranus)) seconds,
            bench "alternative" $ nf (map (altAgeOn Uranus)) seconds
        ],
        bgroup "Neptune" [
            bench "original" $ nf (map (ageOn Neptune)) seconds,
            bench "alternative" $ nf (map (altAgeOn Neptune)) seconds
        ]
    ]
    