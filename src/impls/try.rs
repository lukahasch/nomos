use std::ops::FromResidual;

use crate::parser::lib::Output;

impl<T> std::ops::Try for Output<T> {
    type Output = T;
    type Residual = Output<!>;

    fn from_output(output: Self::Output) -> Self {
        Output::Ok(output)
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            Output::Ok(t) => std::ops::ControlFlow::Continue(t),
            Output::Error(e) => std::ops::ControlFlow::Break(Output::Error(e)),
            Output::Fatal(e) => std::ops::ControlFlow::Break(Output::Fatal(e)),
        }
    }
}

impl<T> FromResidual for Output<T> {
    fn from_residual(residual: <Self as std::ops::Try>::Residual) -> Self {
        match residual {
            Output::Error(e) => Output::Error(e),
            Output::Fatal(e) => Output::Fatal(e),
        }
    }
}
