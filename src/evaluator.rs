use crate::parser::{Expression, TopLevelApplication};
use crate::scope::Scope;
use std::cell::RefCell;
use std::rc::Rc;
use std::{fmt, result};

#[derive(Eq, Debug, PartialEq)]
pub(crate) enum Evaluation {
    Int(i64),
    Function {
        arg_names: Vec<String>,
        body: Expression,
        scope: Rc<RefCell<Scope>>,
    },
    TopScopeFunction {
        name: String,
        function: TopScopeFunction,
    },
}

impl fmt::Display for Evaluation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Evaluation::Int(value) => write!(f, "{}", value),
            Evaluation::Function { .. } => write!(f, "[function]"),
            Evaluation::TopScopeFunction { ref name, .. } => {
                write!(f, "[top scope function: {}]", name)
            }
        }
    }
}

pub(crate) type TopScopeFunction = fn(Vec<Rc<Evaluation>>) -> EvaluationResult<Evaluation>;

pub(crate) fn top_scope_function(
    name: &str,
    function: TopScopeFunction,
) -> (String, Rc<Evaluation>) {
    (
        String::from(name),
        Rc::new(Evaluation::TopScopeFunction {
            name: String::from(name),
            function,
        }),
    )
}

pub(crate) type EvaluationResult<T> = result::Result<Rc<T>, String>;

pub(crate) fn evaluation_result<T>(value: T) -> EvaluationResult<T> {
    Ok(Rc::new(value))
}

fn evaluate_definition(
    scope: Rc<RefCell<Scope>>,
    identifier: String,
    expression: &Expression,
) -> EvaluationResult<Evaluation> {
    let evaluation = evaluate_expression(scope.clone(), &expression)?;
    scope
        .borrow_mut()
        .insert_evaluation(identifier, evaluation.clone());
    Ok(evaluation)
}

fn evaluation_for_identifier(
    scope: Rc<RefCell<Scope>>,
    identifier: &str,
) -> EvaluationResult<Evaluation> {
    match scope.borrow().get_evaluation(&identifier) {
        Some(evaluation) => Ok(evaluation),
        None => Err(format!("Undefined binding: \"{}\"", identifier)),
    }
}

pub(crate) fn evaluate_expression(
    scope: Rc<RefCell<Scope>>,
    expression: &Expression,
) -> EvaluationResult<Evaluation> {
    match *expression {
        Expression::Number(value) => evaluation_result(Evaluation::Int(value)),
        Expression::Word(ref name) => evaluation_for_identifier(scope, name),
        Expression::Application(ref operator, ref arguments) => {
            let operator = evaluate_expression(scope.clone(), operator)?;
            evaluate_application(scope, &operator, arguments)
        }
        Expression::TopLevelApplication(ref operator, ref arguments) => {
            evaluate_top_level_application(scope, operator, arguments)
        }
    }
}

fn evaluate_top_level_application(
    scope: Rc<RefCell<Scope>>,
    operator: &TopLevelApplication,
    expressions: &[Expression],
) -> EvaluationResult<Evaluation> {
    match operator {
        TopLevelApplication::Do => evaluate_do(scope, expressions),
        TopLevelApplication::Define => {
            if expressions.len() != 2 {
                return Err(format!(
                    "Incorrect use of define. Expected 2 expressions, got {}",
                    expressions.len()
                ));
            }
            match &expressions[0] {
                Expression::Word(name) => match expressions.get(1) {
                    Some(expression) => evaluate_definition(scope, String::from(name), expression),
                    None => Err(format!(
                        "Incorrect use of define. No expression at index 1 in: {:#?}",
                        expressions
                    )),
                },
                _ => Err(format!(
                    "Incorrect use of define: Expression 0 is not a word in: {:#?}",
                    expressions
                )),
            }
        }
        TopLevelApplication::Fun => {
            let split = match expressions.split_last() {
                Some(result) => result,
                None => return Err(String::from("Functions need a body")),
            };

            let body = split.0;

            let args = split.1;

            let mut arg_names: Vec<String> = Vec::new();

            for arg in args {
                match arg {
                    Expression::Word(name) => arg_names.push(String::from(name)),
                    _ => return Err(format!("Parameter names must be words. Got: {:#?}", arg)),
                }
            }

            let evaluation = Rc::new(Evaluation::Function {
                arg_names,
                body: body.clone(),
                scope,
            });
            Ok(evaluation)
        }
    }
}

fn evaluate_application(
    scope: Rc<RefCell<Scope>>,
    operator: &Evaluation,
    expressions: &[Expression],
) -> EvaluationResult<Evaluation> {
    match *operator {
        Evaluation::Function {
            ref arg_names,
            ref body,
            scope: ref function_scope,
        } => {
            if expressions.len() != arg_names.len() {
                Err(format!(
                    "Wrong number of arguments. Expected {}, got {}",
                    arg_names.len(),
                    expressions.len()
                ))
            } else {
                let mut evaluations = vec![];
                for (index, identifier) in arg_names.iter().enumerate() {
                    let evaluated = evaluate_expression(scope.clone(), &expressions[index])?;
                    match *evaluated.clone() {
                        Evaluation::Int(_) => {}
                        _ => return Err(format!("Unexpected function argument: {}", evaluated)),
                    }
                    evaluations.push((identifier.clone(), evaluated));
                }
                let local_scope = Rc::new(RefCell::new(Scope::wrap(
                    evaluations,
                    function_scope.clone(),
                )));
                evaluate_expression(local_scope, body)
            }
        }
        Evaluation::TopScopeFunction { function, .. } => {
            let mut evaluations = vec![];
            for expr in expressions {
                evaluations.push(evaluate_expression(scope.clone(), expr)?);
            }
            function(evaluations)
        }
        _ => Err(format!("Applying a non-function: {}", operator)),
    }
}

fn evaluate_do(scope: Rc<RefCell<Scope>>, block: &[Expression]) -> EvaluationResult<Evaluation> {
    let mut result = Rc::new(Evaluation::Int(0));

    for stmt in block {
        result = evaluate_expression(scope.clone(), stmt)?;
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::combinators::Parser;
    use crate::evaluator::{evaluate_expression, Evaluation};
    use crate::parser::{expression, Expression};
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn simple_function() -> std::result::Result<(), String> {
        let program = "
        (do
          ((fun c c) 5))
    ";
        let parsed = expression().parse(program)?;
        assert_eq!(parsed.0, "");
        let scope = Rc::new(RefCell::new(Scope::new()));
        let evaluated = evaluate_expression(scope, &parsed.1)?;
        assert_eq!(evaluated, Rc::new(Evaluation::Int(5)));
        Ok(())
    }

    #[test]
    fn function_scope() -> std::result::Result<(), String> {
        let program = "
        (do
            (define a 8)
            (define function
                (fun a (fun a)))

            ((function 5)))
    ";
        let parsed = expression().parse(program)?;
        assert_eq!(parsed.0, "");
        let scope = Rc::new(RefCell::new(Scope::new()));
        let evaluated = evaluate_expression(scope, &parsed.1)?;
        assert_eq!(evaluated, Rc::new(Evaluation::Int(5)));
        Ok(())
    }

    #[test]
    fn outer_scope() -> std::result::Result<(), String> {
        let program = "
        (do
          (define c 5)
          ((fun c)))
    ";
        let parsed = expression().parse(program)?;
        assert_eq!(parsed.0, "");
        let scope = Rc::new(RefCell::new(Scope::new()));
        let evaluated = evaluate_expression(scope, &parsed.1)?;
        assert_eq!(evaluated, Rc::new(Evaluation::Int(5)));
        Ok(())
    }

    #[test]
    fn parent_scope() -> std::result::Result<(), String> {
        let scope_evaluations = vec![(String::from("abc"), Rc::new(Evaluation::Int(8)))];

        let scope = Rc::new(RefCell::new(Scope::from(scope_evaluations)));
        let expression = Expression::Application(
            Box::new(Expression::TopLevelApplication(
                TopLevelApplication::Fun,
                vec![Expression::Word(String::from("abc"))],
            )),
            vec![],
        );

        let evaluated = evaluate_expression(scope, &expression)?;
        assert_eq!(evaluated, Rc::new(Evaluation::Int(8)));
        Ok(())
    }
}
