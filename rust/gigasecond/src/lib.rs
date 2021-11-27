use chrono::{DateTime, Utc};
use num_format::{Locale, ToFormattedString};

const DURATION_IN_SECONDS: i64 = 1_000_000_000;

/// Returns a Utc DateTime one billion seconds after start.
/// 
/// ## Panics
/// This function will panic on overflow.
pub fn after(start: DateTime<Utc>) -> DateTime<Utc> {
    start
        .checked_add_signed(chrono::Duration::seconds(DURATION_IN_SECONDS))
        .unwrap_or_else(|| {
            panic!(
                "
			Overflow while adding duration: 
			source DateTime was {}, 
			so {} seconds later is too late
",
                start,
                DURATION_IN_SECONDS.to_formatted_string(&Locale::en)
            )
        })
}
