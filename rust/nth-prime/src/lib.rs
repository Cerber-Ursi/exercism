use std::num::NonZeroU32;

pub fn nth(n: u32) -> u32 {
    Generator::new()
        .nth(n as usize)
        .expect("Requested number is too large")
}

trait PrimeGenerator {
    fn last(&self) -> Option<NonZeroU32>;
    
}

#[derive(Debug)]
pub struct Generator {
    cur: Option<NonZeroU32>,
}

impl Generator {
    fn new() -> Self {
        Generator {
            cur: NonZeroU32::new(1),
        }
    }
}

impl Iterator for Generator {
    type Item = u32;
    fn next(&mut self) -> Option<u32> {
        self.cur = self
            .cur
            .map(|cur| {
                (u32::from(cur)+1..=std::u32::MAX)
                    .into_iter()
                    .skip_while(|num| (2..*num).into_iter().any(|div| num % div == 0))
                    .next()
            })
            .unwrap_or(None)
            .map(NonZeroU32::new)
            .unwrap_or(None);
        self.cur.map(u32::from)
    }
}

#[cfg(feature = "memoed")]
#[derive(Debug, Clone)]
pub struct Memoed {
    memo: Vec<(NonZeroU64, Option<NonZeroU64>)>,
}

#[cfg(feature = "memoed")]
impl Memoed {
    pub fn new() -> Memoed {
        Memoed { memo: vec![] }
    }
}

#[cfg(feature = "memoed")]
impl Iterator for Memoed {
    type Item = u32;
    fn next(&mut self) -> u32 {
        let last = self.memo.last().unwrap_or((1, 1)).0;
        let next = (last..=std::u32::MAX).into_iter().skip_while(|i| i > 0);
    }
}
