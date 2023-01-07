use std::{
    collections::{HashMap, HashSet},
    iter::Peekable,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Functions {
    Sqrt,
    Cbrt,
    Root,
    Log,
    Ln,
    Custom((String, usize)),
}

impl Functions {
    pub fn get_args(&self) -> usize {
        match self {
            Functions::Root | Functions::Log => 2,
            Functions::Sqrt | Functions::Cbrt | Functions::Ln => 1,
            Functions::Custom((_, num)) => *num,
        }
    }

    pub fn from_string(str: &str) -> Option<Self> {
        match str {
            "sqrt" => Some(Functions::Sqrt),
            "cbrt" => Some(Functions::Cbrt),
            "root" => Some(Functions::Root),
            "log" => Some(Functions::Log),
            "ln" => Some(Functions::Ln),
            _ => None,
        }
    }

    pub fn get_all() -> Vec<&'static str> {
        vec!["sqrt", "cbrt", "root", "log", "ln"]
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Tokens {
    Plus,
    UnaryPlus,
    Minus,
    UnaryMinus,
    Multiply,
    Divide,
    Modulo,
    Power,
    Factorial,
    Function(Functions),
    ArgsSeparator,
    ParenOpen,
    ParenClose,
    Equals,
    Number(u32),
    Var(String),
    FuncName(String),
    FuncArgs(usize),
    FuncArg(usize),
}

impl Tokens {
    pub fn tokenize_string(
        str: &str,
        functions: Option<&HashMap<String, (SyntaxTree, usize)>>,
        vars: Option<&HashMap<String, SyntaxTree>>,
    ) -> Result<Vec<Self>, String> {
        if str.len() > 8 && &str[..8] == "function" {
            let (name, other) = str[9..].split_once('(').ok_or("Can't find name")?;
            let (args, body) = other.split_once(')').ok_or("Can't find args")?;
            let args = &HashSet::from_iter(args.split(", ").collect::<Vec<&str>>());
            let mut ret = vec![
                Tokens::FuncName(name.to_string()),
                Tokens::FuncArgs(args.len()),
            ];

            ret.append(&mut Self::tokenize_function_body(
                body, functions, args,
            ));

            Ok(ret)
        } else {
            Ok(Self::tokenize_equation(str, functions, vars))
        }
    }

    fn tokenize_equation(
        str: &str,
        functions: Option<&HashMap<String, (SyntaxTree, usize)>>,
        vars: Option<&HashMap<String, SyntaxTree>>,
    ) -> Vec<Self> {
        let mut symbols: Vec<Tokens> = vec![];
        let mut chars = str.chars().into_iter().peekable();

        while let Some(char) = chars.next() {
            let symbol = match char {
                '+' if matches!(symbols.last(), Some(last) if last.is_operator())
                    || symbols.last() == None =>
                {
                    Tokens::UnaryPlus
                }
                '+' => Tokens::Plus,
                '-' if matches!(symbols.last(), Some(last) if last.is_operator())
                    || symbols.last() == None =>
                {
                    Tokens::UnaryMinus
                }
                '-' => Tokens::Minus,
                '*' => Tokens::Multiply,
                '/' => Tokens::Divide,
                '%' => Tokens::Modulo,
                '!' => Tokens::Factorial,
                '^' => Tokens::Power,
                '(' => Tokens::ParenOpen,
                ')' => Tokens::ParenClose,
                '=' => Tokens::Equals,
                ',' => Tokens::ArgsSeparator,
                c if c.is_ascii_digit() => {
                    let mut num = c.to_digit(10).unwrap();

                    while let Some(c) = chars.peek() {
                        if c.is_ascii_digit() {
                            num = num * 10 + chars.next().unwrap().to_digit(10).unwrap();
                        } else {
                            break;
                        }
                    }

                    Tokens::Number(num)
                }
                c if c.is_ascii_alphabetic() => {
                    fn funkyvars(
                        mut str: String,
                        functions: Option<&HashMap<String, (SyntaxTree, usize)>>,
                        vars: Option<&HashMap<String, SyntaxTree>>
                    ) -> (String, Option<Tokens>) {
                        let mut token = None;

                        if let Some(vars) = vars {
                            for key in vars.keys() {
                                if str.len() >= key.len() && &str[..key.len()] == key {
                                    str = str[key.len()..].to_string();
                                    token = Some(Tokens::Var(key.to_string()));
                                    break;
                                }
                            }
                        }

                        if let Some(func) = functions {
                            for key in func.keys() {
                                if str.len() >= key.len() && &str[..key.len()] == key {
                                    str = str[key.len()..].to_string();
                                    token = Some(Tokens::Function(Functions::Custom((
                                        key.to_string(),
                                        func.get(key).unwrap().1,
                                    ))));
                                    break;
                                }
                            }
                        }

                        let funcs = Functions::get_all();

                        for key in funcs {
                            if str.len() >= key.len() && &str[..key.len()] == key {
                                str = str[key.len()..].to_string();
                                token =
                                    Some(Tokens::Function(Functions::from_string(key).unwrap()));
                                break;
                            }
                        }

                        (str, token)
                    }

                    let mut cs = vec![c];

                    while let Some(c) = chars.peek() {
                        if c.is_ascii_alphabetic() {
                            cs.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }

                    let (mut str, mut token) = funkyvars(cs.into_iter().collect(), functions, vars);

                    if str.is_empty() {
                        if let Some(token) = token {
                            token
                        } else {
                            panic!("this shouldn't happen")
                        }
                    } else {
                        if token.is_some() {
                            panic!("this also shouldn't happen")
                        } else {
                            while !str.is_empty() {
                                let ch = str.remove(0);

                                if matches!(symbols.last(), Some(token) if !token.is_operator()) {
                                    symbols.push(Tokens::Multiply)
                                }
                                symbols.push(Tokens::Var(ch.to_string()));

                                (str, token) = funkyvars(str, functions, vars);
                            }

                            if let Some(token) = token {
                                if matches!(symbols.last(), Some(token) if !token.is_operator()) {
                                    symbols.push(Tokens::Multiply)
                                }
                                token
                            } else {
                                continue
                            }
                        }
                    }
                }
                _ => continue,
            };
            symbols.push(symbol);
        }

        symbols
    }

    fn tokenize_function_body(
        str: &str,
        functions: Option<&HashMap<String, (SyntaxTree, usize)>>,
        args: &HashSet<&str>,
    ) -> Vec<Self> {
        let mut symbols: Vec<Tokens> = vec![];
        let mut chars = str.chars().into_iter().peekable();

        while let Some(char) = chars.next() {
            let symbol = match char {
                '+' if matches!(symbols.last(), Some(last) if last.is_operator())
                    || symbols.last() == None =>
                {
                    Tokens::UnaryPlus
                }
                '+' => Tokens::Plus,
                '-' if matches!(symbols.last(), Some(last) if last.is_operator())
                    || symbols.last() == None =>
                {
                    Tokens::UnaryMinus
                }
                '-' => Tokens::Minus,
                '*' => Tokens::Multiply,
                '/' => Tokens::Divide,
                '%' => Tokens::Modulo,
                '!' => Tokens::Factorial,
                '^' => Tokens::Power,
                '(' => Tokens::ParenOpen,
                ')' => Tokens::ParenClose,
                '=' => Tokens::Equals,
                ',' => Tokens::ArgsSeparator,
                c if c.is_ascii_digit() => {
                    let mut num = c.to_digit(10).unwrap();

                    while let Some(c) = chars.peek() {
                        if c.is_ascii_digit() {
                            num = num * 10 + chars.next().unwrap().to_digit(10).unwrap();
                        } else {
                            break;
                        }
                    }

                    Tokens::Number(num)
                }
                c if c.is_ascii_alphabetic() => {
                    fn funkyargs(
                        mut str: String,
                        functions: Option<&HashMap<String, (SyntaxTree, usize)>>,
                        args: &HashSet<&str>,
                    ) -> (String, Option<Tokens>) {
                        let mut token = None;

                        for (i, key) in args.iter().enumerate() {
                            if str.len() >= key.len() && &str[..key.len()] == *key {
                                str = str[key.len()..].to_string();
                                token = Some(Tokens::FuncArg(i));
                                break;
                            }
                        }

                        if let Some(func) = functions {
                            for key in func.keys() {
                                if str.len() >= key.len() && &str[..key.len()] == key {
                                    str = str[key.len()..].to_string();
                                    token = Some(Tokens::Function(Functions::Custom((
                                        key.to_string(),
                                        func.get(key).unwrap().1,
                                    ))));
                                    break;
                                }
                            }
                        }

                        let funcs = Functions::get_all();

                        for key in funcs {
                            if str.len() >= key.len() && &str[..key.len()] == key {
                                str = str[key.len()..].to_string();
                                token =
                                    Some(Tokens::Function(Functions::from_string(key).unwrap()));
                                break;
                            }
                        }

                        (str, token)
                    }

                    let mut cs = vec![c];

                    while let Some(c) = chars.peek() {
                        if c.is_ascii_alphabetic() {
                            cs.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }

                    let (mut str, mut token) = funkyargs(cs.into_iter().collect(), functions, args);

                    if str.is_empty() {
                        if let Some(token) = token {
                            token
                        } else {
                            panic!("this shouldn't happen")
                        }
                    } else {
                        if token.is_some() {
                            panic!("this also shouldn't happen")
                        } else {
                            while !str.is_empty() {
                                let ch = str.remove(0);

                                if matches!(symbols.last(), Some(token) if !token.is_operator()) {
                                    symbols.push(Tokens::Multiply)
                                }
                                symbols.push(Tokens::Var(ch.to_string()));

                                (str, token) = funkyargs(str, functions, args);
                            }

                            if let Some(token) = token {
                                if matches!(symbols.last(), Some(token) if !token.is_operator()) {
                                    symbols.push(Tokens::Multiply)
                                }
                                token
                            } else {
                                continue
                            }
                        }
                    }
                }
                _ => continue,
            };
            symbols.push(symbol);
        }

        symbols
    }

    fn is_operator(&self) -> bool {
        match self {
            Tokens::Equals
            | Tokens::Power
            | Tokens::Factorial
            | Tokens::Multiply
            | Tokens::Divide
            | Tokens::Modulo
            | Tokens::Plus
            | Tokens::UnaryPlus
            | Tokens::Minus
            | Tokens::UnaryMinus => true,
            _ => false,
        }
    }

    fn is_prefix_unary(&self) -> bool {
        match self {
            Tokens::UnaryPlus | Tokens::UnaryMinus => true,
            _ => false,
        }
    }

    fn is_postfix_unary(&self) -> bool {
        match self {
            Tokens::Factorial => true,
            _ => false,
        }
    }

    fn op_level(&self) -> usize {
        match self {
            Tokens::Function(_) => 4,
            Tokens::Power | Tokens::Factorial => 3,
            Tokens::Multiply | Tokens::Divide | Tokens::Modulo => 2,
            Tokens::Plus | Tokens::Minus => 1,
            _ => 0,
        }
    }
}

enum Type {
    Expression,
    Function,
    Variable,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SyntaxTree {
    value: Tokens,
    nodes: Vec<Self>,
}

impl SyntaxTree {
    fn get(tokens: Vec<Tokens>) -> Result<(Type, Self), String> {
        Parser::<std::vec::IntoIter<Tokens>>::parse(tokens)
    }

    fn solve(&self) -> i64 {
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

    fn new(value: Tokens) -> Self {
        SyntaxTree {
            value,
            nodes: vec![],
        }
    }

    fn nodes(mut self, nodes: Vec<SyntaxTree>) -> Self {
        self.nodes = nodes;
        self
    }

    fn inline_vars(&mut self, vars: &HashMap<String, SyntaxTree>) {
        if let Tokens::Var(var) = &self.value {
            if let Some(ast) = vars.get(var) {
                *self = (*ast).clone();
            }
        }
        for node in &mut self.nodes {
            node.inline_vars(&vars)
        }
    }
}

#[derive(Debug)]
struct Parser<T: Iterator<Item = Tokens> + std::fmt::Debug> {
    iterator: Peekable<T>,
}

impl<T: Iterator<Item = Tokens> + std::fmt::Debug> Parser<T> {
    // Grammar:
    // start      -> EXP | EXP "=" EXP | FuncName FuncArgs EXP
    // EXP        -> TERM1 T1
    // TERM1      -> TERM2 T2
    // TERM2      -> FUNCTION T3
    // T1         -> OP1 TERM1 T1 | epsilon
    // T2         -> OP2 TERM2 T2 | epsilon
    // T3         -> OP3 PARENS T3 | epsilon
    // FUNCTION   -> OPFUNC? PARENS
    // PARENS     -> OPBEUNARY? (number | "(" EXP{"," EXP}? ")") OPAFUNARY?
    // OP1        -> "+" | "-"
    // OP2        -> "*" | "/" | "%"
    // OP3        -> "^"
    // OPBEUNARY  -> "+" | "-"
    // OPAFUNARY  -> "!"
    // OPFUNC     -> functions eg. sqrt
    fn parse(tokens: Vec<Tokens>) -> Result<(Type, SyntaxTree), String> {
        let mut tokens = Parser {
            iterator: tokens.into_iter().peekable(),
        };

        if matches!(tokens.peek(), Some(Tokens::FuncName(_))) {
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
                    Err(format!("Extra tokens {tokens:#?}"))
                }
            } else {
                Err(format!("Expected FuncArgs, got {tokens:#?}"))
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
                Err(format!("Extra tokens {tokens:#?}"))
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
                "Expected PrefixUnary or Number or Var or ParenOpen got {peek:#?}"
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
                "Expected PrefixUnary or Number or Var or ParenOpen got {peek:#?}"
            ))
        }
    }

    fn term2(&mut self) -> Result<SyntaxTree, String> {
        let peek = self.peek();

        if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Number(_) | Tokens::Var(_) | Tokens::FuncArg(_) | Tokens::ParenOpen | Tokens::Function(_)))
        {
            let function = self.function()?;
            Ok(self.t3(function)?)
        } else {
            Err(format!(
                "Expected PrefixUnary or Number or Var or ParenOpen got {peek:#?}"
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
                    "Expected PrefixUnary or Number or Var or ParenOpen got {peek:#?}"
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
                    "Expected PrefixUnary or Number or Var or ParenOpen got {peek:#?}"
                ))
            }
        } else {
            Ok(prev)
        }
    }

    fn t3(&mut self, mut prev: SyntaxTree) -> Result<SyntaxTree, String> {
        if matches!(self.peek(), Some(token) if token.op_level() == 3) {
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
                    "Expected PrefixUnary or Number or Var or ParenOpen got {peek:#?}"
                ))
            }
        } else {
            Ok(prev)
        }
    }

    fn function(&mut self) -> Result<SyntaxTree, String> {
        let peek = self.peek();

        if matches!(peek, Some(Tokens::Function(_))) {
            Ok(SyntaxTree::new(self.next().unwrap()).nodes(self.parens()?))
        } else if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::ParenOpen | Tokens::Number(_) | Tokens::Var(_) | Tokens::FuncArg(_)))
        {
            Ok(self.parens()?.into_iter().next().unwrap())
        } else {
            Err(format!(
                "Expected Function or PrefixUnary or Number or Var or ParenOpen got {peek:#?}"
            ))
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
        let mut token = if matches!(
            peek,
            Some(Tokens::Number(_) | Tokens::Var(_) | Tokens::FuncArg(_))
        ) {
            vec![SyntaxTree::new(self.next().unwrap())]
        } else if matches!(peek, Some(Tokens::ParenOpen)) {
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
                return Err(format!("Expected ParenOpen got {peek:#?}"));
            }
        } else {
            return Err(format!(
                "Expected PrefixUnary or Number or Var or ParenOpen got {peek:#?}"
            ));
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

pub struct Repl {
    inputs: Vec<String>,
    asts: Vec<SyntaxTree>,
    answers: Vec<Option<Vec<Tokens>>>,
    vars: Option<HashMap<String, SyntaxTree>>,
    functions: Option<HashMap<String, (SyntaxTree, usize)>>,
}

impl Repl {
    pub fn new(
        vars: Option<HashMap<String, SyntaxTree>>,
        functions: Option<HashMap<String, (SyntaxTree, usize)>>,
    ) -> Repl {
        Repl {
            inputs: vec![],
            asts: vec![],
            answers: vec![],
            vars,
            functions,
        }
    }

    pub fn input(&mut self, input: String) -> Result<(), String> {
        match SyntaxTree::get(Tokens::tokenize_string(
            &input,
            self.functions.as_ref(),
            self.vars.as_ref(),
        )?)? {
            (Type::Expression, ast) => self.asts.push(ast),
            (Type::Function, ast) => self.asts.push(ast),
            _ => (),
        };

        Ok(self.inputs.push(input))
    }

    pub fn test_last_ast(&self) {
        println!("{:#?}", self.asts.last());
    }

    pub fn test_tokens(input: String) {
        println!("{:#?}", Tokens::tokenize_string(&input, None, None));
    }

    pub fn solve(&mut self) {
        // if let Some(vars) = &self.vars {
        //     self.ast.inline_vars(&vars);
        // }
        // println!("{:#?}", self.ast);
        // println!("{}", self.ast.solve());
    }
}
