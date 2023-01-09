pub mod parser;

use std::collections::HashMap;

use parser::{
    ast::{self, Inliner, SyntaxTree, Type},
    tokenizer::Tokens,
    Parser,
};

pub struct Repl {
    inputs: Vec<(String, Option<i64>)>,
    asts: Vec<SyntaxTree>,
    answers: Vec<Option<Vec<Tokens>>>,
    vars: Option<HashMap<String, SyntaxTree>>,
    functions: Option<HashMap<String, (SyntaxTree, usize)>>,
}

impl Repl {
    pub fn new(vars: ast::Vars, functions: ast::Functions) -> Repl {
        Repl {
            inputs: vec![],
            asts: vec![],
            answers: vec![],
            vars: vars.0,
            functions: functions.0,
        }
    }

    pub fn input(&mut self, input: String) -> Result<(), String> {
        match Parser::parse_string(&input, self.vars.as_ref(), self.functions.as_ref())? {
            (Type::Expression, ast) => self.asts.push({
                let ast = if let Some(vars) = &self.vars {
                    ast.inline_vars(&Inliner::Vars(vars))?
                } else {
                    ast
                };

                if let Some(functions) = &self.functions {
                    ast.inline_functions(functions)?
                } else {
                    ast
                }
            }),
            (Type::Function, mut ast) => {
                let func = (
                    match ast.value {
                        Tokens::FuncName(name) => name,
                        _ => panic!("something went wrong"),
                    },
                    (
                        std::mem::replace(&mut ast.nodes[1], SyntaxTree::new(Tokens::Plus)),
                        match &ast.nodes[0].value {
                            Tokens::FuncArgs(args) => *args,
                            got => panic!("something went wrong {got:#?}"),
                        },
                    ),
                );

                if let Some(functions) = &mut self.functions {
                    functions.insert(func.0, func.1);
                } else {
                    self.functions = Some(HashMap::from([(func.0, func.1)]));
                }
            }
            (Type::Variable, mut ast) => {
                let var = (
                    match ast.value {
                        Tokens::VarName(name) => name,
                        _ => panic!("something went wrong"),
                    },
                    std::mem::replace(&mut ast.nodes[0], SyntaxTree::new(Tokens::Plus)),
                );

                if let Some(vars) = &mut self.vars {
                    vars.insert(var.0, var.1);
                } else {
                    self.vars = Some(HashMap::from([(var.0, var.1)]))
                }
            }
        };

        Ok(self
            .inputs
            .push((input, self.asts.last().map(|ast| ast.solve()))))
    }

    pub fn test_last_ast(&self) {
        println!("{:#?}", self.asts.last());
    }

    pub fn test_tokens(input: String) {
        println!("{:#?}", Tokens::tokenize_string(&input, None, None));
    }

    pub fn test_ast(input: String) {
        println!("{:#?}", Parser::parse_string(&input, None, None).unwrap().1);
    }
}
