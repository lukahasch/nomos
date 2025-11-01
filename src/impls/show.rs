use crate::{Normalized, Pattern, Show, Term, Type, egraph::ClassId, parser::Parsed};
use std::ops::Deref;

impl Show for Term<Parsed> {
    #[allow(clippy::explicit_deref_methods)]
    fn show(&self, ctx: &crate::Context, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            fmt,
            "{}",
            match self {
                Term::LangItem(item) => format!("(LangItem {item:?})",),
                Term::Error(e) => format!("(Error {e})"),
                Term::Type(ty) => format!("(Type {})", ctx.show(ty)),
                Term::Integer(i) => format!("{i}"),
                Term::Float(f) => format!("{f}"),
                Term::List(a) => format!(
                    "(List {})",
                    a.iter()
                        .map(|v| ctx.show(v.deref()))
                        .collect::<Vec<_>>()
                        .join(" ")
                ),
                Term::Block(a) => format!(
                    "(Block {})",
                    a.iter()
                        .map(|v| ctx.show(v.deref()))
                        .collect::<Vec<_>>()
                        .join(" ")
                ),
                Term::Application { function, argument } => {
                    format!(
                        "(App {} {})",
                        ctx.show(function.deref().deref()),
                        ctx.show(argument.deref().deref())
                    )
                }
                Term::Function { pattern, body } => {
                    format!(
                        "(Fun {} {})",
                        ctx.show(pattern.deref().deref()),
                        ctx.show(body.deref().deref())
                    )
                }
                Term::Variable(v) => format!("(Var {})", v.item),
                Term::Let {
                    pattern,
                    value,
                    body,
                } => format!(
                    "(Let {} {} {})",
                    ctx.show(pattern.deref().deref()),
                    ctx.show(value.deref().deref()),
                    body.as_ref()
                        .map_or("None".to_string(), |b| ctx.show(b.deref().deref()))
                ),
                Term::Define {
                    pattern,
                    value,
                    body,
                } => format!(
                    "(Def {} {} {})",
                    ctx.show(pattern.deref().deref()),
                    ctx.show(value.deref().deref()),
                    body.as_ref()
                        .map_or("None".to_string(), |b| ctx.show(b.deref().deref()))
                ),
                Term::If {
                    condition,
                    then,
                    r#else,
                } => format!(
                    "(If {} {} {})",
                    ctx.show(condition.deref().deref()),
                    ctx.show(then.deref().deref()),
                    ctx.show(r#else.deref().deref())
                ),
                Term::Match { value, branches } => format!(
                    "(Match {} {})",
                    ctx.show(value.deref().deref()),
                    branches
                        .iter()
                        .map(|(p, t)| format!(
                            "({} {})",
                            ctx.show(p.deref().deref()),
                            ctx.show(t.deref().deref())
                        ))
                        .collect::<Vec<_>>()
                        .join(" ")
                ),
                Term::Inference(i) => format!("(Inference {i})"),
            }
        )
    }
}

impl Show for Pattern<Parsed> {
    #[allow(clippy::explicit_deref_methods)]
    fn show(&self, ctx: &crate::Context, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            fmt,
            "{}",
            match self {
                Pattern::Error(e) => format!("(Error {e})"),
                Pattern::Typed { pattern, ty } => format!(
                    "(Typed {} {})",
                    ctx.show(pattern.deref().deref()),
                    ctx.show(ty.deref().deref())
                ),
                Pattern::Wildcard => "_".to_string(),
                Pattern::Capture(v) => format!("(Capture {})", v.item),
                Pattern::Rest => "...".to_string(),
                Pattern::List(a) => format!(
                    "(List {})",
                    a.iter()
                        .map(|v| ctx.show(v.deref().deref()))
                        .collect::<Vec<_>>()
                        .join(" ")
                ),
                Pattern::As { pattern, name } =>
                    format!("(As {} {})", ctx.show(pattern.deref().deref()), name.item),
                Pattern::If { pattern, condition } => {
                    format!(
                        "(If {} {})",
                        ctx.show(pattern.deref().deref()),
                        ctx.show(condition.deref().deref())
                    )
                }
                Pattern::Integer(i) => format!("{i}"),
                Pattern::Float(f) => format!("{f}"),
            }
        )
    }
}

impl Show for Type<Parsed> {
    #[allow(clippy::explicit_deref_methods)]
    fn show(&self, ctx: &crate::Context, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            fmt,
            "{}",
            match self {
                Type::Error(e) => format!("(Error {e})"),
                Type::Variable(v) => format!("(Var {})", v.item),
                Type::Function { parameter, result } => format!(
                    "(Fun {} {})",
                    ctx.show(parameter.deref().deref()),
                    ctx.show(result.deref().deref())
                ),
            }
        )
    }
}

impl Show for Term<Normalized> {
    #[allow(clippy::explicit_deref_methods)]
    fn show(&self, ctx: &crate::Context, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Error(e) => write!(fmt, "(Error {e})"),
            Term::Integer(i) => write!(fmt, "{i}"),
            Term::Float(f) => write!(fmt, "{f}"),
            Term::List(a) => write!(
                fmt,
                "(List {})",
                a.iter().map(|v| ctx.show(v)).collect::<Vec<_>>().join(" ")
            ),
            Term::Block(a) => write!(
                fmt,
                "(Block {})",
                a.iter().map(|v| ctx.show(v)).collect::<Vec<_>>().join(" ")
            ),
            Term::Type(ty) => write!(fmt, "(Type {})", ctx.show(ty)),
            Term::Application { function, argument } => {
                write!(fmt, "(App {} {})", ctx.show(function), ctx.show(argument))
            }
            Term::Function { pattern, body } => write!(
                fmt,
                "(Fun {} {})",
                ctx.show(pattern.deref().deref()),
                ctx.show(body)
            ),
            Term::Variable(v) => write!(fmt, "(Var {v})"),
            Term::Let {
                pattern,
                value,
                body,
            } => write!(
                fmt,
                "(Let {} {} {})",
                ctx.show(pattern.deref().deref()),
                ctx.show(value),
                body.as_ref().map_or("None".to_string(), |b| ctx.show(b))
            ),
            Term::Define {
                pattern,
                value,
                body,
            } => write!(
                fmt,
                "(Def {} {} {})",
                ctx.show(pattern.deref().deref()),
                ctx.show(value),
                body.as_ref().map_or("None".to_string(), |b| ctx.show(b))
            ),
            Term::If {
                condition,
                then,
                r#else,
            } => write!(
                fmt,
                "(If {} {} {})",
                ctx.show(condition),
                ctx.show(then),
                ctx.show(r#else)
            ),
            Term::Match { value, branches } => write!(
                fmt,
                "(Match {} {})",
                ctx.show(value),
                branches
                    .iter()
                    .map(|(p, t)| format!("({} {})", ctx.show(p.deref().deref()), ctx.show(t)))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Term::LangItem(item) => write!(fmt, "(LangItem {item:?})"),
            Term::Inference(i) => write!(fmt, "(Inference {i})"),
        }
    }
}

impl Show for Pattern<Normalized> {
    #[allow(clippy::explicit_deref_methods)]
    fn show(&self, ctx: &crate::Context, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Error(e) => write!(fmt, "(Error {e})"),
            Pattern::Wildcard => write!(fmt, "_"),
            Pattern::Integer(i) => write!(fmt, "{i}"),
            Pattern::Float(f) => write!(fmt, "{f}"),
            Pattern::Typed { pattern, ty } => {
                write!(
                    fmt,
                    "(Typed {} {})",
                    ctx.show(pattern.deref().deref()),
                    ctx.show(ty)
                )
            }
            Pattern::List(a) => write!(
                fmt,
                "(List {})",
                a.iter()
                    .map(|v| ctx.show(v.deref().deref()))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Pattern::Capture(v) => write!(fmt, "(Capture {v})"),
            Pattern::Rest => write!(fmt, "..."),
            Pattern::As { pattern, name } => {
                write!(fmt, "(As {} {name})", ctx.show(pattern.deref().deref()))
            }
            Pattern::If { pattern, condition } => {
                write!(
                    fmt,
                    "(If {} {})",
                    ctx.show(pattern.deref().deref()),
                    ctx.show(condition)
                )
            }
        }
    }
}

impl Show for Type<Normalized> {
    #[allow(clippy::explicit_deref_methods)]
    fn show(&self, ctx: &crate::Context, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Error(e) => write!(fmt, "(Error {e})"),
            Type::Variable(v) => write!(fmt, "(Var {v:?})"),
            Type::Function { parameter, result } => {
                write!(fmt, "(Fun {} {})", ctx.show(parameter), ctx.show(result))
            }
        }
    }
}

impl Show for ClassId {
    fn show(&self, ctx: &crate::Context, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            fmt,
            "{}@{:?}",
            ctx.show(ctx.egraph.extract_class(*self, ()).unwrap().deref()),
            self.0.into_raw_parts(),
        )
    }
}
