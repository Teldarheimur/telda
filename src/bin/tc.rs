use telda::tc::*;
use telda::{s, program};

pub fn hello_world() -> Program {
    program![
        Item::StaticVariable(VariableDeclaration {
            constant: true,
            name: s!("hello"),
            var_type: Type::Array(Box::new(Type::Byte), None),
            value: Some(Expr::Value(LiteralValue::String(s!("Hello, World!")))),
        }),
        Item::Function {
            return_type: Type::Int,
            name: s!("main"),
            arguments: vec![],
            body: Block {
                statements: vec![
                    Statement::If(If{condition: Expr::Value(LiteralValue::Byte(1)), block: Block {
                        statements: vec![
                            Statement::Expr(Expr::FunctionApplication(s!("print"), vec![Expr::Variable(s!("hello"))])),
                        ]
                    }, else_: None}),
                    Statement::Expr(Expr::FunctionApplication(s!("print"), vec![Expr::Variable(s!("hello"))])),
                ]
            }
        }
    ]
}

fn main() {
    println!("{:#}", DisplayCode(hello_world()));
}
