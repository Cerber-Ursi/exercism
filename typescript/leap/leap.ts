// Naive and most obvious implementation, just to test that it works at all.
function isLeapYear(year: number) {
    if (year % 400 === 0) { return true }
    if (year % 100 === 0) { return false }
    if (year % 4 === 0) { return true }
    return false
}

// Reversed order of checks, to return early more often. Speedup by about 10-15%.
// Ridiculous here (it's about 30k ops/sec anyway), but should be remembered in more complex cases.
export function isLeapYearReversed(year: number) {
    if (year % 4 !== 0) { return false }
    if (year % 100 !== 0) { return true }
    if (year % 400 !== 0) { return false }
    return true
}

// Trying to eliminate explicit checks, hoping for short-circuit to do its work.
// No speedup at all, average the same as with naive approach.
export function isLeapYearNoIfs(year: number) {
    return (year % 400 === 0) || (year % 100 !== 0) && (year % 4 === 0)
}

// Canonical solution, according to mentor's notes.
// Here we're doing both the first (reversing order) and the second (explicit checks elimination)
// modifications to the naive one.
// Works the same as simply reversed solution (i.e. about 10% speedup from the naive).
export function isLeapYearNoIfsReversed(year: number) {
    return year % 4 === 0 && (year % 100 !== 0 || year % 400 === 0)
}

// Trying to eliminate explicit comparisons, hoping for late conversion to do its work.
// In fact, this couldn't work, since conversions aren't late - they appear on negation.
// And really, this is the same as naive approach.
export function isLeapYearNoCompare(year: number) {
    return !(year % 400) || ((year % 100) && !(year % 4))
}

export default isLeapYear