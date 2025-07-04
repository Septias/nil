use std::fmt;
use thiserror::Error;
/// An error that occured during type inference.
#[derive(Debug, Error)]
pub enum InferError {
    #[error("Unknown identifier")]
    UnknownIdentifier,
    #[error("Unknown inherit")]
    UnknownInherit,
    #[error("The record field {field} is missing")]
    MissingRecordField { field: String },
    #[error("Cannot constrain {lhs} <: {rhs}")]
    CannotConstrain { lhs: Ty, rhs: Ty },
    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch { expected: TypeName, found: TypeName },
    #[error("Can't convert {from} to {to}")]
    ConversionError { from: String, to: &'static str },
    #[error("Can't infer type of comment")]
    UnexpectedComment,
    #[error("The supplied argument has too many fields {field}")]
    TooManyField { field: String },
    #[error("Unknown function call")]
    UnknownFunction,
    #[error("Function has to accept at least one argument")]
    TooFewArguments,
    #[error("Multiple")]
    MultipleErrors(Vec<SpannedError>),
    #[error(transparent)]
    Other(#[from] anyhow::Error),

    #[error(transparent)]
    Parser(#[from] parser::ParseError),
}

impl PartialEq for InferError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::TypeMismatch {
                    expected: l_expected,
                    found: l_found,
                },
                Self::TypeMismatch {
                    expected: r_expected,
                    found: r_found,
                },
            ) => l_expected == r_expected && l_found == r_found,
            (
                Self::ConversionError {
                    from: l_from,
                    to: l_to,
                },
                Self::ConversionError {
                    from: r_from,
                    to: r_to,
                },
            ) => l_from == r_from && l_to == r_to,
            (Self::MultipleErrors(l0), Self::MultipleErrors(r0)) => l0 == r0,
            (Self::Other(l0), Self::Other(r0)) => l0.to_string() == r0.to_string(),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl InferError {
    pub fn span(self, span: &Span) -> SpannedError {
        SpannedError {
            span: span.clone(),
            error: self,
        }
    }
}

/// An Error that also contains the span in the source.
#[derive(Debug, Error, PartialEq)]
pub struct SpannedError {
    pub span: Span,
    pub error: InferError,
}

impl fmt::Display for SpannedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} at [{}, {}]",
            self.error, self.span.start, self.span.end
        )
    }
}

/// Type infercence result.
pub type InferResult<T> = Result<T, InferError>;

/// Create an [InferResult].
pub(crate) fn infer_error<T>(expected: TypeName, found: TypeName) -> InferResult<T> {
    Err(InferError::TypeMismatch { expected, found })
}

/// [InferResult] with a [Span] attached.
pub type SpannedInferResult<T> = Result<T, SpannedError>;

/// Create a [SpannedInferResult].
pub fn spanned_infer_error<T>(
    expected: TypeName,
    found: TypeName,
    span: &Span,
) -> SpannedInferResult<T> {
    Err(SpannedError {
        error: InferError::TypeMismatch { expected, found },
        span: span.clone(),
    })
}

impl From<(Span, TypeName, TypeName)> for SpannedError {
    fn from((span, expected, found): (Span, TypeName, TypeName)) -> Self {
        Self {
            span,
            error: InferError::TypeMismatch { expected, found },
        }
    }
}

impl From<(String, &'static str)> for InferError {
    fn from((from, to): (String, &'static str)) -> Self {
        InferError::ConversionError { from, to }
    }
}

impl From<(&Span, InferError)> for SpannedError {
    fn from((span, error): (&Span, InferError)) -> Self {
        Self {
            span: span.clone(),
            error,
        }
    }
}
