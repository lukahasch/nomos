&str -> [Token]

[Token] -> Value

Unbound('x')

Solve(Equation, [Variables]) -> Result(Map(Symbol, Expression), SolveError)

Overlay { TypeId -> Option }

let add_one = fn(x) -> x + 1
> None
type(add_one)
> forall t0 where t0 <: Add(Number) t0 -> t0::Add(Number)::Output
