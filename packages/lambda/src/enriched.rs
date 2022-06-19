use crate::ConcreteValue;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expr {
    // e.g., `4`
    Constant(ConcreteValue),
    // e.g., `cons`
    Builtin(String),
    // e.g., `x`
    Variable(String),
    // e.g., `(f x)`
    Application(Box<Expr>, Box<Expr>),
    // e.g., `(λx. + x x)`
    Lambda(Box<Pattern>, Box<Expr>),
    // e.g., `let x = 3 in (* x 2)`
    Let(LetArm, Box<Expr>),
    // e.g., `letrec x = 4, y = (* 6 x) in (+ x y)`
    LetRec(Vec<LetArm>, Box<Expr>),
    // e.g., `expr1 || expr2`
    FatBar(Box<Expr>, Box<Expr>),
    // e.g., `case tr of (tree-node left right) => 2, (leaf-node value) => 1`
    Case(String, Vec<MatchArm>),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Pattern {
    // e.g., `17`
    Constant(ConcreteValue),
    // e.g., `w`
    Variable(String),
    // e.g., `(tree-node left right)`
    Constructor(String, Vec<Pattern>),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct LetArm(Box<Pattern>, Box<Expr>);

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct MatchArm(Box<Pattern>, Box<Expr>);

/*

letrec
    tree-sum = (λtr.
        case tr in
            (tree-node left right) => (+ (tree-sum left) (tree-sum right))
            (leaf-node value) => value
    )
in
    tree-sum (tree-node (leaf-node 1) (tree-node (leaf-node 2) (leaf-node 3)))

<:=:>

Expr::LetRec(
    vec![
        LetArm(
            Pattern::Variable("tree-sum"),
            Expr::Lambda(
                Pattern::Variable("tr"),
                Expr::Case(
                    "tr",
                    vec![
                        MatchArm(
                            Pattern::Constructor(
                                "tree-node",
                                vec![
                                    Pattern::Variable("left"),
                                    Pattern::Variable("right"),
                                ],
                            ),
                            Expr::Application(
                                Expr::Application(
                                    Expr::Builtin("+"),
                                    Expr::Application(
                                        Expr::Variable("tree-sum"),
                                        Expr::Variable("left"),
                                    ),
                                ),
                                Expr::Application(
                                    Expr::Variable("tree-sum"),
                                    Expr::Variable("right"),
                                ),
                            ),
                        ),
                        MatchArm(
                            Pattern::Constructor(
                                "leaf-node",
                                vec![
                                    Pattern::Variable("value"),
                                ],
                            ),
                            Expr::Variable("value"),
                        ),
                    ],
                ),
            ),
        ),
    ],
    Expr::Application(
        Expr::Variable("tree-sum"),
        Expr::Application(
            Expr::Application(
                Expr::Variable("tree-node"),
                Expr::Application(
                    Expr::Variable("leaf-node"),
                    Expr::Constant(ConcreteValue::Int(1)),
                ),
            ),
            Expr::Application(
                Expr::Application(
                    Expr::Variable("tree-node"),
                    Expr::Application(
                        Expr::Variable("leaf-node"),
                        Expr::Constant(ConcreteValue::Int(2)),
                    ),
                ),
                Expr::Application(
                    Expr::Variable("leaf-node"),
                    Expr::Constant(ConcreteValue::Int(3)),
                ),
            ),
        ),
    ),
)

*/