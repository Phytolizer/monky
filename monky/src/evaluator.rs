use crate::ast;
use crate::ast::Expression;
use crate::ast::Statement;
use crate::object::Integer;
use crate::object::Object;

pub fn eval(node: ast::Node) -> Option<Box<dyn Object>> {
    match node {
        ast::Node::Program(p) => eval_statements(p.statements),
        ast::Node::ExpressionStatement(e) => eval(match e.expression {
            Some(Expression::IntegerLiteral(i)) => ast::Node::IntegerLiteral(i),
            _ => return None,
        }),
        ast::Node::IntegerLiteral(i) => Some(Box::new(Integer { value: i.value })),
        _ => None,
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
        check("5", expect![[r#"
            Some(
                Integer {
                    value: 5,
                },
            )"#]]);
        check("10", expect![[r#"
            Some(
                Integer {
                    value: 10,
                },
            )"#]]);
    }
}
