module Main where

import LeapYear (isLeapYear, isLeapYearRem, isLeapYearRemReversed)
import Criterion
import Criterion.Main

main = defaultMain 
    [ 
        -- uncomment this to benchmark every implementation independently
        bgroup "single mod" [
            bench "1996" $ nf isLeapYear 1996,
            bench "1997" $ nf isLeapYear 1997,
            bench "2000" $ nf isLeapYear 2000
        ],
        bgroup "single rem" [
            bench "1996" $ nf isLeapYearRem 1996,
            bench "1997" $ nf isLeapYearRem 1997,
            bench "2000" $ nf isLeapYearRem 2000
        ], 
        bgroup "single rem reversed" [
            bench "1996" $ nf isLeapYearRemReversed 1996,
            bench "1997" $ nf isLeapYearRemReversed 1997,
            bench "2000" $ nf isLeapYearRemReversed 2000
        ], 
        bgroup "list" [
            bench "mod" $ nf (map isLeapYear) [1..2000],
            bench "rem" $ nf (map isLeapYearRem) [1..2000],
            bench "rem reversed" $ nf (map isLeapYearRemReversed) [1..2000]
        ]
    ]
