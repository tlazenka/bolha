use crate::combinators::{
    either, left, match_literal, pair, whitespace_wrap, zero_or_more, ParseResult, Parser,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Expression {
    Number(i64),
    Word(String),
    Application(Box<Expression>, Vec<Expression>),
    TopLevelApplication(TopLevelApplication, Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TopLevelApplication {
    Do,
    Define,
    Fun,
}

impl Expression {
    fn make_leaf_expression(value: String) -> Expression {
        match value.parse::<i64>() {
            Ok(number) => Expression::Number(number),
            Err(_) => Expression::Word(value),
        }
    }

    fn make_application_expression(operator: Expression, arguments: Vec<Expression>) -> Expression {
        if let Expression::Word(name) = &operator {
            match &name[..] {
                "do" => return Expression::TopLevelApplication(TopLevelApplication::Do, arguments),
                "define" => {
                    return Expression::TopLevelApplication(TopLevelApplication::Define, arguments)
                }
                "fun" => {
                    return Expression::TopLevelApplication(TopLevelApplication::Fun, arguments)
                }
                _ => {}
            }
        }
        Expression::Application(Box::new(operator), arguments)
    }
}

fn identifier(input: &str) -> ParseResult<String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if (next != '(') && (next != ')') && (!next.is_whitespace()) => {
            matched.push(next)
        }
        _ => return Err(input),
    }

    for next in chars {
        if (next != '(') && (next != ')') && (!next.is_whitespace()) {
            matched.push(next);
        } else {
            break;
        }
    }

    let next_index = matched.len();
    Ok((&input[next_index..], matched))
}

fn leaf_expression<'a>() -> impl Parser<'a, Expression> {
    identifier.map(Expression::make_leaf_expression)
}

fn application_expression<'a>() -> impl Parser<'a, Expression> {
    whitespace_wrap(match_literal("(").and_then(|_| {
        left(
            pair(
                whitespace_wrap(expression()),
                zero_or_more(whitespace_wrap(expression())),
            )
            .map(move |(el, children)| Expression::make_application_expression(el, children)),
            match_literal(")"),
        )
    }))
}

pub(crate) fn expression<'a>() -> impl Parser<'a, Expression> {
    either(leaf_expression(), application_expression())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parsing() -> std::result::Result<(), String> {
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
        assert_eq!(
            parsed.1,
            Expression::TopLevelApplication(
                TopLevelApplication::Do,
                vec![
                    Expression::TopLevelApplication(
                        TopLevelApplication::Define,
                        vec![
                            Expression::Word(String::from("adder")),
                            Expression::TopLevelApplication(
                                TopLevelApplication::Fun,
                                vec![
                                    Expression::Word(String::from("a")),
                                    Expression::TopLevelApplication(
                                        TopLevelApplication::Fun,
                                        vec![
                                            Expression::Word(String::from("b")),
                                            Expression::Application(
                                                Box::new(Expression::Word(String::from("+"))),
                                                vec![
                                                    Expression::Word(String::from("a")),
                                                    Expression::Word(String::from("b")),
                                                ]
                                            )
                                        ]
                                    )
                                ]
                            )
                        ]
                    ),
                    Expression::TopLevelApplication(
                        TopLevelApplication::Define,
                        vec![
                            Expression::Word(String::from("add_5")),
                            Expression::Application(
                                Box::new(Expression::Word(String::from("adder"))),
                                vec![Expression::Number(5),]
                            )
                        ]
                    ),
                    Expression::Application(
                        Box::new(Expression::Word(String::from("print"))),
                        vec![Expression::Application(
                            Box::new(Expression::Word(String::from("add_5"))),
                            vec![Expression::Number(3),]
                        )]
                    ),
                ]
            )
        );

        Ok(())
    }
}
