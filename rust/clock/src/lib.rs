use std::ops::Add;
use std::fmt::{self, Formatter, Display};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Hours(u8);

impl Hours {
    fn from_i32(input: i32) -> Self {
        Self(((input as u32) % 24) as u8)
    }
}

impl Add<i32> for Hours {
    type Output = Self;
    fn add(self, other: i32) -> Self {
        Self::from_i32(self.0 as i32 + other % 24)
    }
}

impl Display for Hours {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Minutes(u8);

impl Minutes {
    fn from_i32(input: i32) -> (Self, i32) {
        (Self(((input as u32) % 60) as u8), input / 60)
    }
}

impl Add<i32> for Minutes {
    type Output = (Self, i32);
    fn add(self, _other: i32) -> (Self, i32) {
        unimplemented!();
    }
}

impl Display for Minutes {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Clock(Hours, Minutes);

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        let (minutes, wrap) = Minutes::from_i32(minutes);
        let hours = Hours::from_i32(hours) + wrap;
        Self(hours, minutes)
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        let (minutes, wrap) = self.1 + minutes;
        let hours = self.0 + wrap;
        Self(hours, minutes)
    }
}

impl Display for Clock {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:02}:{:02}", self.0, self.1)?;
        Ok(())
    }
}
