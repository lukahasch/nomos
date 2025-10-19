use crate::{Pattern, Show, Term, Type, parser::Parsed};
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
