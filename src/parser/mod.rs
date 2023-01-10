pub mod ast;
pub mod tokenizer;

use std::{collections::HashMap, iter::Peekable};

use crate::{SyntaxTree, Type};
use tokenizer::Tokens;

#[derive(Debug)]
pub(crate) struct Parser {
    iterator: Peekable<std::vec::IntoIter<Tokens>>,
}

impl Parser {
    pub(crate) fn parse_string(
        string: &String,
        vars: Option<&HashMap<String, SyntaxTree>>,
        functions: Option<&HashMap<String, (SyntaxTree, usize)>>,
    ) -> Result<(Type, SyntaxTree), String> {
        Parser::parse(Tokens::tokenize_string(string, functions, vars)?)
    }

    // start      -> EXP | EXP "=" EXP | FuncName FuncArgs EXP | VarName EXP
    // EXP        -> TERM1 T1
    // TERM1      -> TERM2 T2
    // TERM2      -> TERM3 T3
    // TERM3      -> FUNCTION T4
    // T1         -> OP1 TERM1 T1 | epsilon
    // T2         -> OP2 TERM2 T2 | epsilon
    // T3         -> OP3 TERM3 T3 | epsilon
    // T4         -> OP4 PARENS T4 | epsilon
    // FUNCTION   -> OPFUNC? PARENS
    // PARENS     -> OPBEUNARY? (FUNCTION | number | "(" EXP{"," EXP}? ")") OPAFUNARY?
    // OP1        -> "+" | "-"
    // OP2        -> "*" | "%"
    // OP3        -> "/"
    // OP4        -> "^"
    // OPBEUNARY  -> "+" | "-"
    // OPAFUNARY  -> "!"
    // OPFUNC     -> functions eg. sqrt
    pub(crate) fn parse(tokens: Vec<Tokens>) -> Result<(Type, SyntaxTree), String> {
        let mut tokens = Parser {
            iterator: tokens.into_iter().peekable(),
        };
        let peek = tokens.peek();

        if matches!(peek, Some(Tokens::FuncName(_))) {
            let name = tokens.next().unwrap();
            if matches!(tokens.peek(), Some(Tokens::FuncArgs(_))) {
                let args = tokens.next().unwrap();
                let value = tokens.exp()?;

                if tokens.peek() == None {
                    Ok((
                        Type::Function,
                        SyntaxTree::new(name).nodes(vec![SyntaxTree::new(args), value]),
                    ))
                } else {
                    Err(format!(
                        "Extra tokens {:#?}",
                        tokens.iterator.collect::<Vec<Tokens>>()
                    ))
                }
            } else {
                Err(format!("Expected FuncArgs, got {tokens:#?}"))
            }
        } else if matches!(peek, Some(Tokens::VarName(_))) {
            let name = tokens.next().unwrap();
            let value = tokens.exp()?;

            if tokens.peek() == None {
                Ok((Type::Variable, SyntaxTree::new(name).nodes(vec![value])))
            } else {
                Err(format!(
                    "Extra tokens {:#?}",
                    tokens.iterator.collect::<Vec<Tokens>>()
                ))
            }
        } else {
            let value = tokens.exp()?;
            let peek = tokens.peek();

            if peek == None {
                Ok((Type::Expression, value))
            } else if peek == Some(&Tokens::Equals) {
                Ok((
                    Type::Expression,
                    SyntaxTree::new(tokens.next().unwrap()).nodes(vec![value, tokens.exp()?]),
                ))
            } else {
                Err(format!(
                    "Extra tokens {:#?}",
                    tokens.iterator.collect::<Vec<Tokens>>()
                ))
            }
        }
    }

    fn exp(&mut self) -> Result<SyntaxTree, String> {
        let peek = self.peek();

        if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Number(_) | Tokens::Var(_) | Tokens::FuncArg(_) | Tokens::ParenOpen | Tokens::Function(_)))
        {
            let term1 = self.term1()?;
            Ok(self.t1(term1)?)
        } else {
            Err(format!(
                "exp: Expected PrefixUnary or Number or Var or ParenOpen got {peek:?}"
            ))
        }
    }

    fn term1(&mut self) -> Result<SyntaxTree, String> {
        let peek = self.peek();

        if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Number(_) | Tokens::Var(_) | Tokens::FuncArg(_) | Tokens::ParenOpen | Tokens::Function(_)))
        {
            let term2 = self.term2()?;
            Ok(self.t2(term2)?)
        } else {
            Err(format!(
                "term1: Expected PrefixUnary or Number or Var or ParenOpen got {peek:?}"
            ))
        }
    }

    fn term2(&mut self) -> Result<SyntaxTree, String> {
        let peek = self.peek();

        if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Number(_) | Tokens::Var(_) | Tokens::FuncArg(_) | Tokens::ParenOpen | Tokens::Function(_)))
        {
            let function = self.term3()?;
            Ok(self.t3(function)?)
        } else {
            Err(format!(
                "term2: Expected PrefixUnary or Number or Var or ParenOpen got {peek:?}"
            ))
        }
    }

    fn term3(&mut self) -> Result<SyntaxTree, String> {
        let peek = self.peek();

        if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Number(_) | Tokens::Var(_) | Tokens::FuncArg(_) | Tokens::ParenOpen | Tokens::Function(_)))
        {
            let function = self.function()?;
            Ok(self.t4(function)?)
        } else {
            Err(format!(
                "term2: Expected PrefixUnary or Number or Var or ParenOpen got {peek:?}"
            ))
        }
    }

    fn t1(&mut self, prev: SyntaxTree) -> Result<SyntaxTree, String> {
        if matches!(self.peek(), Some(token) if token.op_level() == 1) {
            let op = self.next().unwrap();
            let peek = self.peek();

            if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Number(_) | Tokens::Var(_) | Tokens::FuncArg(_) | Tokens::ParenOpen | Tokens::Function(_)))
            {
                let term1 = self.term1()?;
                Ok(self.t1(SyntaxTree::new(op).nodes(vec![prev, term1]))?)
            } else {
                Err(format!(
                    "t1: Expected PrefixUnary or Number or Var or ParenOpen got {peek:?}"
                ))
            }
        } else {
            Ok(prev)
        }
    }

    fn t2(&mut self, prev: SyntaxTree) -> Result<SyntaxTree, String> {
        if matches!(self.peek(), Some(token) if token.op_level() == 2) {
            let op = self.next().unwrap();
            let peek = self.peek();

            if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Number(_) | Tokens::Var(_) | Tokens::FuncArg(_) | Tokens::ParenOpen | Tokens::Function(_)))
            {
                let term2 = self.term2()?;
                Ok(self.t2(SyntaxTree::new(op).nodes(vec![prev, term2]))?)
            } else {
                Err(format!(
                    "t2: Expected PrefixUnary or Number or Var or ParenOpen got {peek:?}"
                ))
            }
        } else {
            Ok(prev)
        }
    }

    fn t3(&mut self, prev: SyntaxTree) -> Result<SyntaxTree, String> {
        if matches!(self.peek(), Some(token) if token.op_level() == 3) {
            let op = self.next().unwrap();
            let peek = self.peek();

            if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Number(_) | Tokens::Var(_) | Tokens::FuncArg(_) | Tokens::ParenOpen | Tokens::Function(_)))
            {
                let term2 = self.term3()?;
                Ok(self.t3(SyntaxTree::new(op).nodes(vec![prev, term2]))?)
            } else {
                Err(format!(
                    "t2: Expected PrefixUnary or Number or Var or ParenOpen got {peek:?}"
                ))
            }
        } else {
            Ok(prev)
        }
    }

    fn t4(&mut self, mut prev: SyntaxTree) -> Result<SyntaxTree, String> {
        if matches!(self.peek(), Some(token) if token.op_level() == 4) {
            let op = self.next().unwrap();
            let peek = self.peek();

            if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Number(_) | Tokens::Var(_) | Tokens::FuncArg(_) | Tokens::ParenOpen | Tokens::Function(_)))
            {
                if prev.value == Tokens::Power {
                    let node = prev.nodes.pop().unwrap();

                    prev.nodes.push(
                        SyntaxTree::new(op)
                            .nodes(vec![node, self.parens()?.into_iter().next().unwrap()]),
                    );

                    Ok(prev)
                } else {
                    let mut v = vec![prev];
                    v.append(&mut self.parens()?);
                    self.t3(SyntaxTree::new(op).nodes(v))
                }
            } else {
                Err(format!(
                    "t3: Expected PrefixUnary or Number or Var or ParenOpen got {peek:?}"
                ))
            }
        } else {
            Ok(prev)
        }
    }

    fn function(&mut self) -> Result<SyntaxTree, String> {
        match self.peek() {
            Some(Tokens::Function(_)) => {
                Ok(SyntaxTree::new(self.next().unwrap()).nodes(self.parens()?))
            }
            Some(token)
                if token.is_prefix_unary()
                    || matches!(
                        token,
                        Tokens::Function(_)
                            | Tokens::ParenOpen
                            | Tokens::Number(_)
                            | Tokens::Var(_)
                            | Tokens::FuncArg(_)
                    ) =>
            {
                Ok(self.parens()?.into_iter().next().unwrap())
            }
            peek => Err(format!(
                "function: Expected Function|PrefixUnary|Number|Var|ParenOpen got {peek:?}"
            )),
        }
    }

    fn parens(&mut self) -> Result<Vec<SyntaxTree>, String> {
        let peek = self.peek();

        let unary = if matches!(peek, Some(token) if token.is_prefix_unary()) {
            self.next()
        } else {
            None
        };

        let peek = self.peek();
        let mut token = match peek {
            Some(Tokens::Function(_)) => vec![self.function()?],
            Some(Tokens::Number(_) | Tokens::Var(_) | Tokens::FuncArgs(_)) => {
                vec![SyntaxTree::new(self.next().unwrap())]
            }
            Some(Tokens::ParenOpen) => {
                self.next();
                let mut value = vec![self.exp()?];

                while let Some(&Tokens::ArgsSeparator) = self.peek() {
                    self.next();
                    value.push(self.exp()?);
                }

                let peek = self.peek();

                if peek == Some(&Tokens::ParenClose) {
                    self.next();
                    value
                } else {
                    return Err(format!("Expected ParenClose got {peek:?}"));
                }
            }
            _ => {
                return Err(format!(
                    "parens: Expected PrefixUnary|Function|Number|Var|ParenOpen got {peek:?}"
                ))
            }
        };

        if matches!(self.peek(), Some(token) if token.is_postfix_unary()) {
            token = vec![SyntaxTree::new(self.next().unwrap()).nodes(token)]
        }

        Ok(if let Some(unary) = unary {
            vec![SyntaxTree::new(unary).nodes(token)]
        } else {
            token
        })
    }

    fn peek(&mut self) -> Option<&Tokens> {
        self.iterator.peek()
    }

    fn next(&mut self) -> Option<Tokens> {
        self.iterator.next()
    }
}
