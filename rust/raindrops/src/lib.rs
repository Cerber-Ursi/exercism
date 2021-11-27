use itertools::Itertools;
use lazy_static::lazy_static;

lazy_static! {
    /// The dictionary of the raindrops language.
    ///
    /// It must be `static`, so that we could use it several times in a row without recreating every time,
    /// if we so wish. The dictionary is a vec of tuples, each consisting of number and a corresponding
    /// word.
    ///
    /// Note that no sorting and no checking for duplicates is performed on translation - you should do
    /// this kind of stuff yourself every time you update the dictionary.
    ///
    /// # Examples
    /// Finding a word by number:
    /// ```
    /// use raindrops::DICTIONARY;
    /// assert_eq!(DICTIONARY.iter().find(|(num, _)| *num == 3).unwrap().1, "Pling");
    /// ```
    /// Reverse translation - finding number by word:
    /// ```
    /// use raindrops::DICTIONARY;
    /// assert_eq!(DICTIONARY.iter().find(|(_, sound)| *sound == "Plong").unwrap().0, 7);
    /// ```
    pub static ref DICTIONARY: Vec<(u32, &'static str)> = vec![
        (3, "Pling"),
        (5, "Plang"),
        (7, "Plong")
    ];
}

fn raindrop(n: u32) -> Option<String> {
    let s = DICTIONARY
        .iter()
        .map(|&(num, sound)| if n % num == 0 { Some(sound) } else { None })
        .filter(Option::is_some)
        .map(Option::unwrap)
        .join("");

    if s.len() > 0 {
        Some(s)
    } else {
        None
    }
}

/// Function performing translation into raindrops language.
///
/// It uses the [DICTIONARY static](struct.DICTIONARY.html) internally.
///
/// # Example
/// ```
/// use raindrops::raindrops;
/// assert_eq!(raindrops(3 * 5 * 7), String::from("PlingPlangPlong"));
/// assert_eq!(raindrops(3 * 5 * 7 + 1), String::from("106"));
/// ```
pub fn raindrops(n: u32) -> String {
    raindrop(n).unwrap_or_else(|| format!("{}", n))
}
