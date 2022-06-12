use std::collections::HashSet;
use std::sync::atomic::{ AtomicUsize, Ordering };

static ALPHA_REPLACEMENT_INDEX: AtomicUsize = AtomicUsize::new(1usize);

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum List {
    Nil,
    Cons(Box<Value>, Box<List>),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Value {
    Boolean(bool),
    Int(usize),
    String(String),
    List(List),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expr {
    Constant(Value),
    Builtin(String),
    Variable(String),
    Application(Box<Expr>, Box<Expr>),
    Lambda(String, Box<Expr>),
}

impl Expr {
    pub fn constant(value: Value) -> Expr { Expr::Constant(value) }

    pub fn builtin(name: &str) -> Expr { Expr::Builtin(name.to_string()) }

    pub fn variable(name: &str) -> Expr { Expr::Variable(name.to_string()) }

    pub fn application(left: Expr, right: Expr) -> Expr {
        Expr::Application(
            Box::new(left),
            Box::new(right),
        )
    }

    pub fn lambda(argname: &str, body: Expr) -> Expr {
        Expr::Lambda(
            argname.to_string(),
            Box::new(body),
        )
    }

    fn collect_free_vars(&self, free_vars: &mut HashSet<String>) {
        match self {
            Expr::Variable(name) => {
                free_vars.insert(name.to_string());
            },
            Expr::Application(left, right) => {
                left.collect_free_vars(free_vars);
                right.collect_free_vars(free_vars);
            },
            Expr::Lambda(argname, body) => {
                body.collect_free_vars(free_vars);
                free_vars.remove(argname);
            },
            _ => {},
        }
    }

    pub fn free_vars(&self) -> HashSet<String> {
        let mut free_vars = HashSet::new();
        self.collect_free_vars(&mut free_vars);
        free_vars
    }

    pub fn has_free_reference(&self, varname: &str) -> bool {
        match self {
            Expr::Variable(name) if name == varname => true,
            Expr::Application(left, right) => {
                left.has_free_reference(varname) || right.has_free_reference(varname)
            },
            Expr::Lambda(argname, body) => {
                (argname != varname) && body.has_free_reference(varname)
            },
            _ => false,
        }
    }

    fn collect_bound_vars(&self, bound_vars: &mut HashSet<String>) {
        match self {
            Expr::Application(left, right) => {
                left.collect_bound_vars(bound_vars);
                right.collect_bound_vars(bound_vars);
            },
            Expr::Lambda(argname, body) => {
                body.collect_bound_vars(bound_vars);
                if body.has_free_reference(argname) {
                    bound_vars.insert(argname.to_string());
                }
            },
            _ => {},
        }
    }

    pub fn bound_vars(&self) -> HashSet<String> {
        let mut bound_vars = HashSet::new();
        self.collect_bound_vars(&mut bound_vars);
        bound_vars
    }

    pub fn has_bound_reference(&self, varname: &str) -> bool {
        match self {
            Expr::Application(left, right) => {
                left.has_bound_reference(varname) || right.has_bound_reference(varname)
            },
            Expr::Lambda(argname, body) => {
                ((argname == varname) && body.has_free_reference(varname)) ||
                body.has_bound_reference(varname)
            },
            _ => false,
        }
    }

    pub fn replace_free(&self, varname: &str, new_expr: &Expr) -> Expr {
        match self {
            Expr::Variable(name) if name == varname => new_expr.clone(),
            Expr::Application(left, right) => {
                Expr::Application(
                    Box::new(left.replace_free(varname, new_expr)),
                    Box::new(right.replace_free(varname, new_expr)),
                )
            },
            Expr::Lambda(argname, body) if (argname != varname) && body.has_free_reference(varname) => {
                if new_expr.has_free_reference(argname) {
                    // To ensure we don't actually bind any of the free variables in `new_expr`,
                    // perform an alpha-substitution on this lambda. For example:
                    // If we're replacing free `y`s with `(+ x 2)` (which has a free `x`) in
                    // the expression `(λx. * y x)`, we don't want the new expression's free `x`,
                    // so we replace the bound one with a new globally unique name. The end result
                    // is something like `(λx_2178. * (+ x 2) x_2178)`.
                    let new_argname = format!("{}_{}", argname, ALPHA_REPLACEMENT_INDEX.fetch_add(1, Ordering::SeqCst));
                    let new_variable = Expr::variable(&new_argname);
                    Expr::Lambda(
                        new_argname,
                        Box::new(
                            body
                                .replace_free(argname, &new_variable)
                                .replace_free(varname, new_expr)
                        )
                    )
                } else {
                    Expr::Lambda(
                        argname.to_string(),
                        Box::new(body.replace_free(varname, new_expr)),
                    )
                }
            },
            _ => self.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{ Expr, Value };

    fn example_expr() -> Expr {
        // (λx. λz. λw. (x y) w) x
        Expr::application(
            Expr::lambda("x",
                Expr::lambda("z",
                    Expr::lambda("w",
                        Expr::application(
                            Expr::application(
                                Expr::variable("x"), Expr::variable("y")
                            ),
                            Expr::variable("w"),
                        )
                    )
                )
            ),
            Expr::variable("x"),
        )
    }

    #[test]
    fn it_finds_free_vars() {
        let expr = example_expr();
        let free_var_set = expr.free_vars();

        assert!(free_var_set.contains("x"));
        assert!(free_var_set.contains("y"));
        assert!(expr.has_free_reference("x"));
        assert!(expr.has_free_reference("y"));
    }

    #[test]
    fn it_finds_bound_vars() {
        let expr = example_expr();
        let bound_var_set = expr.bound_vars();

        assert!(bound_var_set.contains("x"));
        assert!(bound_var_set.contains("w"));
        assert!(!bound_var_set.contains("z"));
        assert!(expr.has_bound_reference("x"));
        assert!(expr.has_bound_reference("w"));
        assert!(!expr.has_bound_reference("z"));
    }

    #[test]
    fn it_performs_substitutions() -> Result<(), String> {
        let expr_1 = Expr::application( // (* y 4)
            Expr::application(
                Expr::builtin("*"),
                Expr::variable("y"),
            ),
            Expr::constant(Value::Int(4)),
        );
        let new_y = Expr::application( // (+ z 2)
            Expr::application(
                Expr::builtin("+"),
                Expr::variable("z"),
            ),
            Expr::constant(Value::Int(2)),
        );

        let new_expr_1 = expr_1.replace_free("y", &new_y);

        assert_eq!(
            Expr::application( // (* (+ z 2) 4)
                Expr::application(
                    Expr::builtin("*"),
                    Expr::application(
                        Expr::application(
                            Expr::builtin("+"),
                            Expr::variable("z"),
                        ),
                        Expr::constant(Value::Int(2)),
                    ),
                ),
                Expr::constant(Value::Int(4)),
            ),
            new_expr_1,
        );

        let expr_2 = Expr::application( // (λy. y) (λx. x) (λx. y) (λz. z y)
            Expr::application(
                Expr::application(
                    Expr::lambda("y", Expr::variable("y")),
                    Expr::lambda("x", Expr::variable("x")),
                ),
                Expr::lambda("x", Expr::variable("y")),
            ),
            Expr::lambda("z", 
                Expr::application(
                    Expr::variable("z"),
                    Expr::variable("y"),
                ),
            ),
        );

        let new_expr_2 = expr_2.replace_free("y", &new_y);

        if let Expr::Application(left, right) = new_expr_2 {
            let expected_simple_branch = Expr::application(
                Expr::application(
                    // We shouldn't replace bound instances of y
                    Expr::lambda("y", Expr::variable("y")),
                    // We shouldn't do anything to expressions with no free instances of y
                    Expr::lambda("x", Expr::variable("x")),
                ),
                // We should replace free instances of y
                Expr::lambda("x", new_y.clone()),
            );
            assert_eq!(*left, expected_simple_branch);

            if let Expr::Lambda(argname, body) = *right {
                assert_ne!("z", argname, "Rightmost lambda argument should have been renamed, but wasn't");
                if let Expr::Application(inner_left, inner_right) = *body {
                    assert_eq!(Expr::variable(&argname), *inner_left);
                    assert_eq!(new_y, *inner_right);
                    Ok(())
                } else {
                    Err("Bad structure: body of rightmost lambda is anot an Application".to_string())
                }
            } else {
                Err("Bad structure: rightmost term is not a Lambda".to_string())
            }
        } else {
            Err("Bad stucture: outermost expression is not an Application".to_string())
        }
    }
}
