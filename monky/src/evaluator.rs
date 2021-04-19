use crate::ast;
use crate::ast::Expression;
use crate::ast::Statement;
use crate::object::Boolean;
use crate::object::Integer;
use crate::object::Object;
use crate::object::ObjectKind;
use crate::object::Value;

pub fn eval(node: ast::Node) -> Option<Box<dyn Object>> {
    match node {
        ast::Node::Program(p) => eval_statements(p.statements),
        ast::Node::ExpressionStatement(e) => eval_expression(e.expression?),
        ast::Node::Expression(e) => eval_expression(e),
        ast::Node::IntegerLiteral(i) => Some(Box::new(Integer { value: i.value })),
        ast::Node::Boolean(b) => Some(Box::new(Boolean { value: b.value })),
        ast::Node::PrefixExpression(p) => {
            let right = eval(ast::Node::Expression(*p.right))?;
            eval_prefix_expression(p.operator, right)
        }
        ast::Node::InfixExpression(i) => {
            let left = eval(ast::Node::Expression(*i.left))?;
            let right = eval(ast::Node::Expression(*i.right))?;
            eval_infix_expression(left, i.operator, right)
        }
        _ => None,
    }
}

fn eval_infix_expression(
    left: Box<dyn Object>,
    operator: String,
    right: Box<dyn Object>,
) -> Option<Box<dyn Object>> {
    if left.kind() == ObjectKind::Integer && right.kind() == ObjectKind::Integer {
        eval_integer_infix_expression(left, operator, right)
    } else if operator == "==" {
        Some(Box::new(Boolean {
            value: left.value() == right.value(),
        }))
    } else if operator == "!=" {
        Some(Box::new(Boolean {
            value: left.value() != right.value(),
        }))
    } else {
        None
    }
}

fn eval_integer_infix_expression(
    left: Box<dyn Object>,
    operator: String,
    right: Box<dyn Object>,
) -> Option<Box<dyn Object>> {
    let left = *left.value().as_integer().unwrap();
    let right = *right.value().as_integer().unwrap();
    match operator.as_str() {
        "+" => Some(Box::new(Integer {
            value: left + right,
        })),
        "-" => Some(Box::new(Integer {
            value: left - right,
        })),
        "*" => Some(Box::new(Integer {
            value: left * right,
        })),
        "/" => Some(Box::new(Integer {
            value: left / right,
        })),
        "<" => Some(Box::new(Boolean {
            value: left < right,
        })),
        ">" => Some(Box::new(Boolean {
            value: left > right,
        })),
        "==" => Some(Box::new(Boolean {
            value: left == right,
        })),
        "!=" => Some(Box::new(Boolean {
            value: left != right,
        })),
        _ => None,
    }
}

fn eval_expression(expression: ast::Expression) -> Option<Box<dyn Object>> {
    eval(match expression {
        Expression::IntegerLiteral(i) => ast::Node::IntegerLiteral(i),
        Expression::Boolean(b) => ast::Node::Boolean(b),
        Expression::Prefix(p) => ast::Node::PrefixExpression(p),
        Expression::Infix(i) => ast::Node::InfixExpression(i),
        _ => return None,
    })
}

fn eval_prefix_expression(operator: String, right: Box<dyn Object>) -> Option<Box<dyn Object>> {
    match operator.as_str() {
        "!" => Some(eval_bang_operator_expression(right)),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => None,
    }
}

fn eval_minus_prefix_operator_expression(right: Box<dyn Object>) -> Option<Box<dyn Object>> {
    if let Value::Integer(i) = right.value() {
        Some(Box::new(Integer { value: -i }))
    } else {
        None
    }
}

fn eval_bang_operator_expression(right: Box<dyn Object>) -> Box<dyn Object> {
    if let Value::Boolean(b) = right.value() {
        Box::new(Boolean { value: !b })
    } else {
        Box::new(Boolean { value: false })
    }
}

fn eval_statements(statements: Vec<Statement>) -> Option<Box<dyn Object>> {
    let mut result = None;
    for statement in statements {
        result = match statement {
            Statement::Expression(e) => eval(ast::Node::ExpressionStatement(e)),
            _ => None,
        };
    }

    result
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    use expect_test::Expect;

    use crate::parser::Parser;

    use super::*;

    fn check(input: &str, expected: Expect) {
        let program = Parser::new(input).parse_program();
        let evaluated = eval(ast::Node::Program(program));
        expected.assert_eq(&format!("{:#?}", evaluated));
    }

    #[test]
    fn eval_integer_expression() {
        check(
            "5",
            expect![[r#"
            Some(
                Integer {
                    value: 5,
                },
            )"#]],
        );
        check(
            "10",
            expect![[r#"
            Some(
                Integer {
                    value: 10,
                },
            )"#]],
        );
    }

    #[test]
    fn eval_boolean_expression() {
        check(
            "true",
            expect![[r#"
            Some(
                Boolean {
                    value: true,
                },
            )"#]],
        );
        check(
            "false",
            expect![[r#"
            Some(
                Boolean {
                    value: false,
                },
            )"#]],
        );
    }

    #[test]
    fn not_true() {
        check(
            "!true",
            expect![[r#"
            Some(
                Boolean {
                    value: false,
                },
            )"#]],
        );
    }
    #[test]
    fn not_false() {
        check(
            "!false",
            expect![[r#"
            Some(
                Boolean {
                    value: true,
                },
            )"#]],
        );
    }
    #[test]
    fn not_five() {
        check(
            "!5",
            expect![[r#"
            Some(
                Boolean {
                    value: false,
                },
            )"#]],
        );
    }
    #[test]
    fn not_not_true() {
        check(
            "!!true",
            expect![[r#"
            Some(
                Boolean {
                    value: true,
                },
            )"#]],
        );
    }
    #[test]
    fn not_not_false() {
        check(
            "!!false",
            expect![[r#"
            Some(
                Boolean {
                    value: false,
                },
            )"#]],
        );
    }
    #[test]
    fn not_not_five() {
        check(
            "!!5",
            expect![[r#"
            Some(
                Boolean {
                    value: true,
                },
            )"#]],
        );
    }
    #[test]
    fn negative_five() {
        check(
            "-5",
            expect![[r#"
            Some(
                Integer {
                    value: -5,
                },
            )"#]],
        );
    }

    #[test]
    fn negative_ten() {
        check(
            "-10",
            expect![[r#"
            Some(
                Integer {
                    value: -10,
                },
            )"#]],
        );
    }

    #[test]
    fn addition() {
        check(
            "5 + 5 + 5 + 5 - 10",
            expect![[r#"
            Some(
                Integer {
                    value: 10,
                },
            )"#]],
        )
    }

    #[test]
    fn multiplication_and_division() {
        check(
            "5 * 6 / 2",
            expect![[r#"
            Some(
                Integer {
                    value: 15,
                },
            )"#]],
        )
    }

    #[test]
    fn parenthesizing() {
        check(
            "(1 + 2) * 3",
            expect![[r#"
            Some(
                Integer {
                    value: 9,
                },
            )"#]],
        )
    }

    #[test]
    fn less_than() {
        check(
            "2 < 3",
            expect![[r#"
            Some(
                Boolean {
                    value: true,
                },
            )"#]],
        )
    }

    #[test]
    fn greater_than() {
        check(
            "2 > 3",
            expect![[r#"
            Some(
                Boolean {
                    value: false,
                },
            )"#]],
        )
    }

    #[test]
    fn equals() {
        check(
            "1 == 1",
            expect![[r#"
            Some(
                Boolean {
                    value: true,
                },
            )"#]],
        )
    }

    #[test]
    fn boolean_equals() {
        check(
            "true == true",
            expect![[r#"
            Some(
                Boolean {
                    value: true,
                },
            )"#]],
        )
    }

    #[test]
    fn boolean_not_equals() {
        check(
            "true != true",
            expect![[r#"
            Some(
                Boolean {
                    value: false,
                },
            )"#]],
        )
    }

    #[test]
    fn comparison_as_bool() {
        check(
            "(1 < 2) == true",
            expect![[r#"
            Some(
                Boolean {
                    value: true,
                },
            )"#]],
        )
    }

    #[test]
    fn not_equals() {
        check(
            "1 != 1",
            expect![[r#"
            Some(
                Boolean {
                    value: false,
                },
            )"#]],
        )
    }
}
