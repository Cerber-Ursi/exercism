//! Library created to help others communicating with Bob.
//! 
//! What, you don't know Bob? I'd say you have lost a lot, especially if you're
//! the junior programmer like myself. Fortunately, you don't have to write the
//! translation routine for communication with him - this routine is right here in this crate.
//! 
//! Of course, if you feel like you want to reimplement all this yourself -
//! we won't stop you in any way. Just remember not to harm Bob.

/// Public type definition for private struct.
///
/// It is held here to be used as guard for [`Message`][Message] enum variants. Since the only field of this structure
/// is private, it is forbidden to construct `Message` from outside this crate.
///
/// For more information, see the documentation on enum itself.
///
/// [Message]: self::Message
pub struct Internal(());

/// Represents the categories of possible Bob's answers.
///
/// Since there is a finite subset of what Bob can say, and it doesn't have any parameters
/// based on the input (we simply choose one way or another, but don't add, for example,
/// any arbitrary number), we can use an enum without any values - the replies are `&'static str`
/// values hard-coded into enum implementation.
///
/// Although, we add some private fields to any variant simply to assure that it won't be
/// constructed in unpredictable way (and to make ourselves free to change this implementation
/// on possible changes of requirements without making a breaking change).
pub enum Message {
    /// Message without any content.
    Silence(Internal),
    /// Any question asked quietly.
    Question(Internal),
    /// Any shouts, excluding questions.
    Shout(Internal),
    /// Any question being yelled.
    ShoutQuestion(Internal),
    /// Default value for all unhandled causes.
    Whatever(Internal),
}

impl Message {
    /// Converts the message string into corresponding enum variant.
    ///
    /// This is one of two ways to construct the `Message` instance from outside this crate.
    /// We must make sure that you won't force Bob to say something if he wasn't asked.
    pub fn incoming(message: &str) -> Self {
        // First, check if the message is nothing. This is the special case,
        // since this check is independent from the others.
        Message::try_as_silence(message)
            // If this returns None, i.e. message is not nothing, we must classify it.
            // We do it simply by calling the dedicated function.
            .unwrap_or_else(|| Self::classify(message))
    }

    /// Consumes enum, handing out the corresponding Message text.
    ///
    /// Of course, you can try and match the `Message` in your own code. We don't take any warranty for what you get,
    /// however, since we can't be sure that you understand Bob as well as we do.
    ///
    /// Really, please, let us translate. Don't think you're the chosen one and can do it yourself.
    pub fn reply(self) -> &'static str {
        use Message::*;
        match self {
            Question(_) => "Sure.",
            Shout(_) => "Whoa, chill out!",
            ShoutQuestion(_) => "Calm down, I know what I'm doing!",
            Silence(_) => "Fine. Be that way!",
            Whatever(_) => "Whatever.",
        }
    }

    // Attempts to instantly classify message as silence.
    //
    // It returns Some(Message.Silence), if the incoming message consist only of whitespace,
    // and None otherwise.
    fn try_as_silence(message: &str) -> Option<Message> {
        if message.trim().len() == 0 {
            Some(Message::Silence(Internal(())))
        } else {
            None
        }
    }

    /// Function to classify non-empty incoming messages.
    /// 
    /// It is used internally by the [incoming] function, but can also be called independently.
    /// 
    /// ## Panics
    /// This function will panic in debug mode if the message is of "silence" class (i.e. contain only whitespace).
    /// In release mode this message will be erroneously classified as [Whatever], be careful!
    /// 
    /// [incoming]: Message::incoming
    /// [Whatever]: Message::Whatever
    pub fn classify(message: &str) -> Message {
        debug_assert!(!message.trim().is_empty());
        let internal = Internal(());
        match (Self::is_shout(message), Self::is_question(message)) {
            (true, true) => Message::ShoutQuestion(internal),
            (true, false) => Message::Shout(internal),
            (false, true) => Message::Question(internal),
            (false, false) => Message::Whatever(internal),
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
        message.trim().chars().last() == Some('?')
    }
}


/// Generates the Bob's reply to incoming message.
/// 
/// This function classifies the messages into the instances of the [`Message`][Message] enum
/// and then asks this enum to hand over the corresponding reply.
/// 
/// [Message]: self::Message
pub fn reply(message: &str) -> &str {
    Message::incoming(message).reply()
}
