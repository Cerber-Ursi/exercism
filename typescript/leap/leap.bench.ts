import * as Benchmark from 'benchmark'
import isLeapYear, { isLeapYearNoCompare, isLeapYearNoIfs, isLeapYearReversed } from './leap'

const years = Object.keys(new Array(2000).fill(undefined)).map((key) => +key)

// Benchmarking is done by checking the whole range from 1 to 2000.
// This eliminates the difference between years and lets us check the average performance.
//
// Example output:
// naive x 29,537 ops/sec ±1.68% (91 runs sampled)
// reversed x 33,543 ops/sec ±0.53% (95 runs sampled)
// no ifs x 30,081 ops/sec ±1.85% (91 runs sampled)
// no compare x 30,363 ops/sec ±0.66% (92 runs sampled)
//
// Run yarn bench to check how this works in your case.
const suite = new Benchmark.Suite('leap years')
suite
    .add('naive', () => years.map(isLeapYear))
    .add('reversed', () => years.map(isLeapYearReversed))
    .add('no ifs', () => years.map(isLeapYearNoIfs))
    .add('no compare', () => years.map(isLeapYearNoCompare))
    .on('cycle', (event: Benchmark.Event) => {
        // tslint:disable:no-console
        console.log(String(event.target))
    })
    .run()