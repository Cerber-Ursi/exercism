module Main where

import SpaceAge
import Criterion
import Criterion.Main

seconds :: [Float]
seconds = map fromIntegral ([100,200..2000] ++ [3000,4000..50000] ++ [100000,105000..1000000])

main = defaultMain 
    [ 
        bgroup "Mercury" [
            bench "canonical" $ nf (map (ageOn Mercury)) seconds,
            bench "recursive" $ nf (map (ageOnRecursive Mercury)) seconds
        ],
        bgroup "Venus" [
            bench "canonical" $ nf (map (ageOn Venus)) seconds,
            bench "recursive" $ nf (map (ageOnRecursive Venus)) seconds
        ],
        bgroup "Earth" [
            bench "canonical" $ nf (map (ageOn Earth)) seconds,
            bench "recursive" $ nf (map (ageOnRecursive Earth)) seconds
        ],
        bgroup "Mars" [
            bench "canonical" $ nf (map (ageOn Mars)) seconds,
            bench "recursive" $ nf (map (ageOnRecursive Mars)) seconds
        ],
        bgroup "Jupiter" [
            bench "canonical" $ nf (map (ageOn Jupiter)) seconds,
            bench "recursive" $ nf (map (ageOnRecursive Jupiter)) seconds
        ],
        bgroup "Saturn" [
            bench "canonical" $ nf (map (ageOn Saturn)) seconds,
            bench "recursive" $ nf (map (ageOnRecursive Saturn)) seconds
        ],
        bgroup "Uranus" [
            bench "canonical" $ nf (map (ageOn Uranus)) seconds,
            bench "recursive" $ nf (map (ageOnRecursive Uranus)) seconds
        ],
        bgroup "Neptune" [
            bench "canonical" $ nf (map (ageOn Neptune)) seconds,
            bench "recursive" $ nf (map (ageOnRecursive Neptune)) seconds
        ]
    ]
    