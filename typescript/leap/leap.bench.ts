import * as Benchmark from 'benchmark'
import isLeapYear, { isLeapYearNoCompare, isLeapYearNoIfs, isLeapYearReversed, isLeapYearNoIfsReversed } from './leap'

const years = Object.keys(new Array(2000).fill(undefined)).map((key) => +key)

// Benchmarking is done by checking the whole range from 1 to 2000.
// This eliminates the difference between years and lets us check the average performance.
//
// Example output:
// naive x 31,477 ops/sec ±1.86% (92 runs sampled)
// reversed x 34,866 ops/sec ±0.19% (95 runs sampled)
// no ifs x 29,309 ops/sec ±2.21% (89 runs sampled)
// no ifs reversed x 34,420 ops/sec ±0.60% (90 runs sampled)
// no compare x 30,843 ops/sec ±1.80% (95 runs sampled)
//
// Run "yarn bench" to check how this works in your case.
const suite = new Benchmark.Suite('leap years')
suite
    .add('naive', () => years.map(isLeapYear))
    .add('reversed', () => years.map(isLeapYearReversed))
    .add('no ifs', () => years.map(isLeapYearNoIfs))
    .add('no ifs reversed', () => years.map(isLeapYearNoIfsReversed))
    .add('no compare', () => years.map(isLeapYearNoCompare))
    .on('cycle', (event: Benchmark.Event) => {
        // tslint:disable:no-console
        console.log(String(event.target))
    })
    .run()