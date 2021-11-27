#[cfg(feature = "memoed")]
use nth_prime::{PrimesMemoed, PrimeGenerator};
use std::time::Instant;

macro_rules! nth {
    ($pm:expr, [$($nth:expr),+]) => {
        let mut pm = $pm;
        $(
            let now = Instant::now();
            let n = $nth;
            let nth = pm.nth(n).unwrap();
            let elapsed = now.elapsed();
            println!("{}-th prime is {}, calculated in {}.{:09} s", n, nth, elapsed.as_secs(), elapsed.subsec_nanos());
        )+
    };
}

#[cfg(feature = "memoed")]
fn main() {
    nth!(PrimesMemoed::new(), [2, 5, 10, 50, 1000, 5000, 10000, 50000, 100000, 1000, 5000, 10000, 50000, 100000]);
    println!("Clearing pimes generator");
    nth!(PrimesMemoed::new(), [100000, 50000, 100001]);
}

#[cfg(not(feature = "memoed"))]
fn main() {
    panic!("Use \"--features memoed\"");
}