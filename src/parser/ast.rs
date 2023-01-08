use std::collections::HashMap;

use crate::parser::Parser;

use super::tokenizer::{self, Tokens};

pub(crate) enum Type {
    Expression,
    Function,
    Variable,
}

pub(crate) enum Inliner<'a> {
    Vars(&'a HashMap<String, SyntaxTree>),
    Args(&'a HashMap<usize, SyntaxTree>),
}

pub struct Vars(pub(crate) Option<HashMap<String, SyntaxTree>>);
pub struct Functions(pub(crate) Option<HashMap<String, (SyntaxTree, usize)>>);

macro_rules! funkyvars {
    ($type:ty) => {
        impl $type {
            pub fn none() -> Self {
                Self(None)
            }
        }
    };
}

funkyvars!(Vars);
funkyvars!(Functions);

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct SyntaxTree {
    pub(crate) value: Tokens,
    pub(crate) nodes: Vec<Self>,
}

impl SyntaxTree {
    pub(crate) fn solve(&self) -> i64 {
        match &self.value {
            Tokens::Equals => {
                if self.nodes[0].solve() == self.nodes[1].solve() {
                    1
                } else {
                    0
                }
            }
            Tokens::Power => self.nodes[0].solve().pow(self.nodes[1].solve() as u32),
            Tokens::Factorial => match self.nodes[0].solve() {
                0 => 1,
                num @ 1.. => (1..num + 1).product(),
                _ => unreachable!(),
            },
            Tokens::Multiply => self.nodes[0].solve() * self.nodes[1].solve(),
            Tokens::Divide => self.nodes[0].solve() / self.nodes[1].solve(),
            Tokens::Modulo => self.nodes[0].solve() % self.nodes[1].solve(),
            Tokens::Plus => self.nodes[0].solve() + self.nodes[1].solve(),
            Tokens::UnaryPlus => self.nodes[0].solve(),
            Tokens::Minus => self.nodes[0].solve() - self.nodes[1].solve(),
            Tokens::UnaryMinus => -self.nodes[0].solve(),
            Tokens::Number(num) => *num as i64,
            token => panic!("{token:?}"),
        }
    }

    pub(crate) fn new(value: Tokens) -> Self {
        SyntaxTree {
            value,
            nodes: vec![],
        }
    }

    pub(crate) fn nodes(mut self, nodes: Vec<SyntaxTree>) -> Self {
        self.nodes = nodes;
        self
    }

    pub(crate) fn inline_vars(mut self, inliner: &Inliner) -> Result<Self, String> {
        match (inliner, &self.value) {
            (Inliner::Vars(vars), Tokens::Var(var)) => {
                if let Some(ast) = vars.get(var) {
                    self = ast.clone()
                }
            }
            (Inliner::Args(args), Tokens::FuncArg(id)) => {
                if let Some(ast) = args.get(id) {
                    self = ast.clone()
                } else {
                    return Err(String::from("Not enough arguments"));
                }
            }
            _ => (),
        };

        let len = self.nodes.len();
        self.nodes = self
            .nodes
            .into_iter()
            .map_while(|node| node.inline_vars(&inliner).ok())
            .collect();

        if len != self.nodes.len() {
            Err(String::from("Not enough arguments"))
        } else {
            Ok(self)
        }
    }

    pub(crate) fn inline_functions(
        mut self,
        functions: &HashMap<String, (SyntaxTree, usize)>,
    ) -> Result<Self, String> {
        let len = self.nodes.len();
        self.nodes = self
            .nodes
            .into_iter()
            .map_while(|node| node.inline_functions(functions).ok())
            .collect();
        if len != self.nodes.len() {
            return Err(String::from("Not enough arguments"));
        }

        if let Tokens::Function(tokenizer::Functions::Custom(name)) = &self.value {
            if let Some((ast, args)) = functions.get(name) {
                self = ast.clone().inline_vars(&Inliner::Args(
                    &self
                        .nodes
                        .into_iter()
                        .take(*args)
                        .enumerate()
                        .collect::<HashMap<usize, SyntaxTree>>(),
                ))?;
            }
        }

        Ok(self)
    }
}
