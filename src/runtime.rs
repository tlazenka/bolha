use crate::evaluator::{evaluation_result, top_scope_function, Evaluation, EvaluationResult};
use crate::scope::Scope;
use std::rc::Rc;

fn top_scope_print(arguments: Vec<Rc<Evaluation>>) -> EvaluationResult<Evaluation> {
    if arguments.len() != 1 {
        return Err(format!(
            "Print only supports 1 argument, got {}",
            arguments.len()
        ));
    }

    let evaluation = match arguments.get(0) {
        Some(result) => result,
        None => return Err(String::from("Print needs an argument")),
    };
    println!("{}", evaluation);
    Ok(evaluation.clone())
}

fn top_scope_plus(arguments: Vec<Rc<Evaluation>>) -> EvaluationResult<Evaluation> {
    let mut result: i64 = 0;
    for arg in arguments {
        match *arg.clone() {
            Evaluation::Int(value) => result += value,
            _ => return Err(format!("Invalid argument: {}", arg)),
        }
    }
    evaluation_result(Evaluation::Int(result))
}

pub(crate) fn init_top_scope() -> Scope {
    let top_scope_evaluations = vec![
        top_scope_function("print", top_scope_print),
        top_scope_function("+", top_scope_plus),
    ];

    Scope::from(top_scope_evaluations)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::combinators::Parser;
    use crate::evaluator::{evaluate_expression, Evaluation};
    use crate::parser::expression;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn evaluation() -> std::result::Result<(), String> {
        let program = "
        (do
            (define adder
                (fun a
                    (fun b
                        (+ a b))))

            (define add_5 (adder 5))

            (print (add_5 3)))
    ";
        let parsed = expression().parse(program)?;
        assert_eq!(parsed.0, "");
        let scope = Rc::new(RefCell::new(init_top_scope()));
        let evaluated = evaluate_expression(scope, &parsed.1)?;
        assert_eq!(evaluated, Rc::new(Evaluation::Int(8)));
        Ok(())
    }
}
