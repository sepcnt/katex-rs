use core::fmt;
use core::ops::Range;

use alloc::borrow::ToOwned as _;
use alloc::sync::Arc;

use crate::types::{ErrorLocationProvider, SourceLocation};

/// Representation for the textual payload of a [`Token`].
///
/// Tokens may either borrow slices from the input stream or own standalone
/// strings that were generated during macro expansion. The enum keeps track of
/// the data's lifetime and enables zero-copy lexing for most tokens.
#[derive(Clone, Debug)]
pub enum TokenText {
    /// Borrowed slice from an input string.
    Slice {
        /// Shared reference to the original input.
        source: Arc<str>,
        /// Range of the slice within the source string.
        range: Range<usize>,
    },
    /// Heap allocated string stored in an [`Arc`].
    Owned(Arc<str>),
    /// Static string literal.
    Static(&'static str),
}

impl TokenText {
    /// Creates a borrowed slice from an input [`Arc<str>`].
    #[must_use]
    pub const fn slice(source: Arc<str>, start: usize, end: usize) -> Self {
        Self::Slice {
            source,
            range: start..end,
        }
    }

    /// Returns the string slice represented by this token.
    #[must_use]
    pub fn as_str(&self) -> &str {
        match self {
            Self::Slice { source, range } => &source[range.clone()],
            Self::Owned(text) => text,
            Self::Static(text) => text,
        }
    }

    /// Returns the length in bytes of the token text.
    #[must_use]
    pub fn len(&self) -> usize {
        self.as_str().len()
    }

    /// Returns `true` when the token text is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Converts the token text into an owned [`String`].
    #[must_use]
    pub fn to_owned_string(&self) -> String {
        self.as_str().to_owned()
    }

    /// Clone into a given string buffer.
    pub fn clone_into(&self, buf: &mut String) {
        buf.clear();
        buf.push_str(self.as_str());
    }
}

impl PartialEq for TokenText {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Slice {
                    source: s1,
                    range: r1,
                },
                Self::Slice {
                    source: s2,
                    range: r2,
                },
            ) => {
                if Arc::ptr_eq(s1, s2) && r1 == r2 {
                    true
                } else {
                    s1[r1.clone()] == s2[r2.clone()]
                }
            }
            (Self::Owned(t1), Self::Owned(t2)) => Arc::ptr_eq(t1, t2) || t1 == t2,
            (Self::Static(t1), Self::Static(t2)) => t1 == t2,
            _ => self.as_str() == other.as_str(),
        }
    }
}

impl Eq for TokenText {}

impl From<String> for TokenText {
    fn from(value: String) -> Self {
        Self::Owned(Arc::from(value))
    }
}

impl From<&str> for TokenText {
    fn from(value: &str) -> Self {
        Self::Owned(Arc::from(value))
    }
}

impl From<Arc<str>> for TokenText {
    fn from(value: Arc<str>) -> Self {
        Self::Owned(value)
    }
}

impl fmt::Display for TokenText {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl From<TokenText> for String {
    fn from(value: TokenText) -> Self {
        match value {
            TokenText::Slice { source, range } => source[range].to_owned(),
            TokenText::Owned(text) => text.as_ref().to_owned(),
            TokenText::Static(text) => text.to_owned(),
        }
    }
}

impl PartialEq<&str> for TokenText {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<TokenText> for &str {
    fn eq(&self, other: &TokenText) -> bool {
        *self == other.as_str()
    }
}

impl PartialEq<String> for TokenText {
    fn eq(&self, other: &String) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<TokenText> for String {
    fn eq(&self, other: &TokenText) -> bool {
        self == other.as_str()
    }
}

/// Represents a single token in the lexing process of LaTeX/KaTeX mathematical
/// expressions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    /// The raw text content of the token as extracted from the input string.
    ///
    /// This preserves the original characters without any processing or
    /// normalization. For commands, this includes the backslash and command
    /// name (e.g., "\\alpha").
    pub text: TokenText,
    /// Optional source location information for error reporting and debugging.
    ///
    /// Provides context about where in the original input this token
    /// originated. Used to generate meaningful error messages when parsing
    /// fails.
    ///
    /// # See Also
    /// - [`SourceLocation`] for location details
    pub loc: Option<SourceLocation>,
    /// Flag indicating whether this token should not be expanded during macro
    /// processing.
    ///
    /// When set to `true`, prevents the parser from applying macro expansions
    /// to this token. This is useful for tokens that should be treated
    /// literally, such as in verbatim environments.
    pub noexpand: Option<bool>,
    /// Flag indicating whether this token should be treated as a `\relax`
    /// command.
    ///
    /// `\relax` is a LaTeX primitive that does nothing but can be used to
    /// prevent unwanted expansions. When this flag is set, the token
    /// behaves as if it were a `\relax` command.
    pub treat_as_relax: Option<bool>,
}

impl Token {
    /// Creates a new token with the specified text and optional source
    /// location.
    #[must_use]
    pub fn new<T>(text: T, loc: Option<SourceLocation>) -> Self
    where
        T: Into<TokenText>,
    {
        Self {
            text: text.into(),
            loc,
            noexpand: None,
            treat_as_relax: None,
        }
    }

    /// Creates a new token borrowing from the provided input slice.
    #[must_use]
    pub const fn from_slice(
        input: Arc<str>,
        start: usize,
        end: usize,
        loc: Option<SourceLocation>,
    ) -> Self {
        Self {
            text: TokenText::slice(input, start, end),
            loc,
            noexpand: None,
            treat_as_relax: None,
        }
    }

    /// Returns the token text as a string slice.
    #[must_use]
    pub fn text(&self) -> &str {
        self.text.as_str()
    }

    /// Mutably sets the textual payload of the token.
    pub fn set_text<T>(&mut self, text: T)
    where
        T: Into<TokenText>,
    {
        self.text = text.into();
    }

    /// Computes a new token that encompasses the range from this token to the
    /// end_token.
    #[must_use]
    pub fn range<T: Into<TokenText>>(self, end_token: Self, text: T) -> Option<Self> {
        let loc = SourceLocation::range(self.loc, end_token.loc)?;
        Some(Self {
            text: text.into(),
            loc: Some(loc),
            noexpand: None,
            treat_as_relax: None,
        })
    }
}

/// Implementation of `ErrorLocationProvider` for `Token`.
///
/// This implementation allows `Token` to be used as an error location
/// provider in the KaTeX parsing pipeline. It simply returns the token's
/// location information for error reporting.
///
/// # Cross-references
///
/// - Part of the error reporting system in
///   `ParseError`(crate::types::ParseError).
/// - Used by parsers to provide location context for syntax errors.
impl ErrorLocationProvider for Token {
    fn loc(&self) -> Option<&SourceLocation> {
        self.loc.as_ref()
    }
}

/// Implementation of `ErrorLocationProvider` for `Option<Token>`.
///
/// This implementation allows optional tokens to be used as error location
/// providers. It delegates to the inner token's location if present.
///
/// # Cross-references
///
/// - Part of the error reporting system in
///   `ParseError`(crate::types::ParseError).
/// - Used when tokens might be absent but location information is still needed.
impl ErrorLocationProvider for Option<Token> {
    fn loc(&self) -> Option<&SourceLocation> {
        let t = self.as_ref()?;
        t.loc.as_ref()
    }
}
