//! Library created to help others communicating with Bob.
//! 
//! What, you don't know Bob? I'd say you have lost a lot, especially if you're
//! the junior programmer like myself. Fortunately, you don't have to write the
//! translation routine for communication with him - this routine is right here in this crate.
//! 
//! Of course, if you feel like you want to reimplement all this yourself -
//! we won't stop you in any way. Just remember not to harm Bob.

/// Represents the categories of possible Bob's answers.
///
/// Since there is a finite subset of what Bob can say, and it doesn't have any parameters
/// based on the input (we simply choose one way or another, but don't add, for example,
/// any arbitrary number), we can use an enum without any values - the replies are `&'static str`
/// values hard-coded into enum implementation.
pub enum Message {
    /// Message without any content.
    Silence,
    /// Any question asked quietly.
    Question,
    /// Any shouts, excluding questions.
    Shout,
    /// Any question being yelled.
    ShoutQuestion,
    /// Default value for all unhandled causes.
    Whatever,
}

impl Message {
    /// Consumes enum, handing out the corresponding Message text.
    ///
    /// Of course, you can try and match the `Message` in your own code. We don't take any warranty for what you get,
    /// however, since we can't be sure that you understand Bob as well as we do.
    ///
    /// Really, please, let us translate. Don't think you're the chosen one and can do it yourself.
    pub fn reply(self) -> &'static str {
        use Message::*;
        match self {
            Question => "Sure.",
            Shout => "Whoa, chill out!",
            ShoutQuestion => "Calm down, I know what I'm doing!",
            Silence => "Fine. Be that way!",
            Whatever => "Whatever.",
        }
    }

    /// Classifies incoming message strings.
    /// 
    /// We strongly encourage you to use this function and not to construct `Message` yourself.
    pub fn classify(message: &str) -> Message {
        if message.trim().is_empty() {
            Message::Silence
                    } else {
        match (Self::is_shout(message), Self::is_question(message)) {
            (true, true) => Message::ShoutQuestion,
            (true, false) => Message::Shout,
            (false, true) => Message::Question,
            (false, false) => Message::Whatever,
        }
        }
    }

    // This function returns true if there are any alphabetic characters and all of them are uppercase.
    // TODO: find the exact requirement. Now it would pass the tests, but this solution looks rather arbitrary here
    fn is_shout(message: &str) -> bool {
        // Intermediate iterator to check for (non-)emptiness
        let mut alphabetic = message.chars().filter(|&c| c.is_alphabetic()).peekable();
        // There must be some character to check...
        alphabetic.peek().is_some() &&
            // ...and the whole iterator must pass this check
            alphabetic.all(|c| c.is_uppercase())
    }

    // This function returns true if the incoming message is a question,
    // i.e. it ends with the question mark.
    fn is_question(message: &str) -> bool {
        message.trim().ends_with('?')
    }
}


/// Generates the Bob's reply to incoming message.
/// 
/// This function classifies the messages into the instances of the [`Message`][Message] enum
/// and then asks this enum to hand over the corresponding reply.
/// 
/// [Message]: self::Message
pub fn reply(message: &str) -> &str {
    Message::classify(message).reply()
}
