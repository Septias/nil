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
    CannotConstrain { lhs: Type, rhs: Type },
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
