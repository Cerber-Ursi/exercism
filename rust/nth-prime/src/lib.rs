#![cfg_attr(all(feature = "nightly", feature = "memoed"), feature(test))]

//! Crate for runtime prime numbers generation.
//! 
//! By default, it provides the `Primes` structure and the `PrimeGenerator` trait.
//! You can opt in touse memoisation by activating "memoed" feature, which exports the 
//! `PrimesMemoed` structure. It works considerably faster for large numbers and for several
//! searches in a row, at the cost of storing all already found primes in memory.

use std::num::NonZeroU32;

/// Basic function to generate the nth prime number. Uses [Primes](struct.Primes.html) as default generator.
///
/// # Panics
/// This function will panic if the requested prime number doesn't fit in the u32 values range.
pub fn nth(n: u32) -> u32 {
    Primes::new()
        .nth(n)
        .expect("Requested number is too large")
        .into()
}

/// General implementation for the prime numbers generating structures.
pub trait PrimeGenerator {
    /// Preducate function to check whether the number is prime.
    ///
    /// Default implementation iterates through all numbers from 2 to num - 1 and checks if num is divisible
    /// by the current number. Generators can override this function for speed.
    fn predicate(&self, num: u32) -> bool {
        num > 2 && (2..num - 1).any(|n| num % n == 0)
    }
    /// Getter function to retrieve the first prime larger then provided number.
    ///
    /// If the generated prime will not fit into u32 range, this method should return None.
    /// Default implementation of this method simply delegates the work to [next_after_pure](#method.next_after_pure) method.
    fn next_after(&mut self, last: u32) -> Option<NonZeroU32> {
        self.next_after_pure(last)
    }
    #[inline]
    /// Pure getter function to retrieve the first prime larger then provided number.
    ///
    /// Implementors should typically use the default implementation of this function, which
    /// simply creates the range from the passed number to [std::u32::max_value()](https://doc.rust-lang.org/std/primitive.u32.html#method.max_value),
    /// skips the non-primes (based on [predicate](#method.predicate)) and calls [next()](https://doc.rust-lang.org/std/iter/trait.Iterator.html#tymethod.next).
    fn next_after_pure(&mut self, last: u32) -> Option<NonZeroU32> {
        (last + 1..=u32::max_value())
            .skip_while(|n| self.predicate(*n))
            .next()
            .and_then(NonZeroU32::new)
    }
    /// Function which generates the nth prime value.
    /// 
    /// By default, it simply delegates the work to [nth_after](#method.nth_after) method.
    fn nth(&mut self, n: u32) -> Option<NonZeroU32> {
        self.nth_after(n, 2)
    }
    /// Function that generates the nth prime value larger then provided number.
    /// 
    /// Implementors should typically use the default implementation of this function, which
    /// simply repeats the call to [next_after](#method.next_after) n times in a row.
    fn nth_after(&mut self, n: u32, value: u32) -> Option<NonZeroU32> {
        (1..=n).fold(NonZeroU32::new(value), |curr, _| curr.and_then(|val| self.next_after(val.into())))
    }
}

/// Default prime number generator.
/// 
/// Its implementation of [PrimeGenerator](trait.PrimeGenerator.html) is just accepting the default methods.
/// For further information, refer to the documentation on the trait itself.
pub struct Primes;
impl Primes {
    fn new() -> Self {
        Primes
    }
}
impl PrimeGenerator for Primes {}

#[cfg(feature = "memoed")]
#[derive(Debug, Clone)]
pub struct PrimesMemoed {
    memo: Vec<u32>,
}

#[cfg(feature = "memoed")]
impl PrimesMemoed {
    pub fn new() -> PrimesMemoed {
        PrimesMemoed {
            memo: vec![2],
        }
    }
}

#[cfg(feature = "memoed")]
impl PrimeGenerator for PrimesMemoed {
    fn predicate(&self, num: u32) -> bool {
        num > 2
            && self
                .memo
                .iter()
                .cloned()
                .any(|div| num % div == 0)
    }
    fn next_after(&mut self, last: u32) -> Option<NonZeroU32> {
        let next = self.next_after_pure(last);
        if let Some(next) = next {
            let next = u32::from(next);
            // self.memo.push((next, next.checked_mul(next).and_then(NonZeroU32::new)));
            self.memo.push(next);
        }
        next
    }
    fn nth(&mut self, n: u32) -> Option<NonZeroU32> {
        let n = n as usize;
        match self.memo.get(n - 1).cloned() {
            Some(val) => NonZeroU32::new(val),
            None => self.nth_after(
                (n - self.memo.len()) as u32,
                self.memo
                    .last()
                    .cloned()
                    .expect("PrimesMemoed structure is corrupted - memo vector is empty")
            ),
        }
    }
}

#[cfg(test)]
#[cfg(feature = "memoed")]
#[cfg(feature = "nightly")]
mod bench {

    extern crate test;
    use crate::{PrimeGenerator, Primes, PrimesMemoed};
    use test::{black_box, Bencher};

    macro_rules! benches {
        ($(($simp:ident, $memoed:ident, $num:expr)),+) => {
            $(
                #[bench]
                fn $simp(b: &mut Bencher) {
                    b.iter(|| Primes::new().nth($num));
                }
                #[bench]
                fn $memoed(b: &mut Bencher) {
                    b.iter(|| PrimesMemoed::new().nth($num));
                }
            )+
        };
    }

    benches!(
        (f_0002_simple, f_0002_memoed, 2),
        (f_0010_simple, f_0010_memoed, 10),
        (f_0050_simple, f_0050_memoed, 50),
        (f_0100_simple, f_0100_memoed, 100),
        (f_0200_simple, f_0200_memoed, 200),
        (f_0500_simple, f_0500_memoed, 500),
        (f_1000_simple, f_1000_memoed, 1000),
        (f_5000_simple, f_5000_memoed, 5000)
    );

    macro_rules! double_benches {
        ($(($simp:ident, $memoed:ident, $num:expr)),+) => {
            $(
                #[bench]
                fn $simp(b: &mut Bencher) {
                    b.iter(|| {
                        let mut primes = black_box(Primes::new());
                        black_box(primes.nth($num));
                        black_box(primes.nth($num));
                    });
                }
                #[bench]
                fn $memoed(b: &mut Bencher) {
                    b.iter(|| {
                        let mut primes = black_box(PrimesMemoed::new());
                        black_box(primes.nth($num));
                        black_box(primes.nth($num));
                    });
                }
            )+
        };
    }

    double_benches!(
        (f_0002_simple_double, f_0002_memoed_double, 2),
        (f_0010_simple_double, f_0010_memoed_double, 10),
        (f_0050_simple_double, f_0050_memoed_double, 50),
        (f_0100_simple_double, f_0100_memoed_double, 100)
    );

}
