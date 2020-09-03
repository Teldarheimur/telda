use telda::tc::*;
use telda::s;

pub fn hello_world() -> Program {
    vec![
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
                    Statement::Expr(Expr::FunctionApplication(s!("print"), vec![Expr::Variable(s!("hello"))])),
                    Statement::Expr(Expr::FunctionApplication(s!("print"), vec![Expr::Variable(s!("hello"))])),
                ]
            }
        }
    ]
}

fn main() {
    println!("{:#}", DisplayCode(hello_world()));
}