module Main where

import Pangram (checkByIterating, checkBySubtracting, checkBySet)
import Criterion
import Criterion.Main

main = defaultMain 
    [ 
        bgroup "iterating" (group checkByIterating),
        bgroup "subtracting" (group checkBySubtracting),
        bgroup "set" (group checkBySet)
    ]

run :: (String -> Bool) -> (String, String) -> Benchmark
run fun (str, name) = bench name $ nf fun str

group :: (String -> Bool) -> [Benchmark]
group fun = map (run fun) [
        ("abcdefghijklmnopqrstuvwxyz", "perfect"),
        ("zyxwvustrqponmlkjihgfdecba", "reverse perfect"),
        ("the quick brown fox jumps over the lazy dog", "classic"),
        ("a quick movement of the enemy will jeopardize five gunboats", "missing 'x'"),
        ("five boxing fighters jump quickly on it", "missing 'a'")
    ]

