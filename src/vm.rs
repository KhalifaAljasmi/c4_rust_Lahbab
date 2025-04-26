/// Virtual Machine module: Executes the compiled/interpreted code.

use crate::parser::ASTNode;

pub fn execute(ast: &[ASTNode]) {
    for node in ast {
        match node {
            ASTNode::Declaration(name, value) => {
                println!("Declare variable: {} with value {}", name, value);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ASTNode;

    #[test]
    fn test_execute_declaration() {
        let ast = vec![ASTNode::Declaration("x".to_string(), 5)];
        execute(&ast);
    }
}
