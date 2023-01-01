use std::{collections::HashMap, iter::Peekable};

#[derive(Debug, PartialEq, Eq, Clone)]
enum Functions {
    Sqrt,
    Cbrt,
    Root,
    Log,
    Ln,
    Custom((String, usize)),
}

impl Functions {
    fn get_args(&self) -> usize {
        match self {
            Functions::Root | Functions::Log => 2,
            Functions::Sqrt | Functions::Cbrt | Functions::Ln => 1,
            Functions::Custom((_, num)) => *num,
        }
    }

    fn from_string(str: &String) -> Option<Self> {
        match str.as_str() {
            "sqrt" => Some(Functions::Sqrt),
            "cbrt" => Some(Functions::Cbrt),
            "root" => Some(Functions::Root),
            "log" => Some(Functions::Log),
            "ln" => Some(Functions::Ln),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Tokens {
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
    Var(char),
}

impl Tokens {
    fn tokenize_string(str: &String, functions: Option<HashMap<String, usize>>) -> Vec<Self> {
        let mut symbols: Vec<Tokens> = vec![];
        let mut chars = str.chars().into_iter().peekable();

        'l: while let Some(char) = chars.next() {
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
                    let mut cs = vec![c];

                    while let Some(c) = chars.peek() {
                        if c.is_ascii_alphabetic() {
                            cs.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }

                    let func_str = cs.iter().collect::<String>();
                    let func = match &functions {
                        Some(functions) => match functions.get(&func_str) {
                            Some(len) => Some(Tokens::Function(Functions::Custom((
                                func_str.to_string(),
                                *len,
                            )))),
                            None => None,
                        },
                        _ => None,
                    };
                    let func = match Functions::from_string(&func_str) {
                        _ if func.is_some() => func,
                        Some(f) => Some(Tokens::Function(f)),
                        None => None,
                    };

                    if let Some(func) = func {
                        if matches!(symbols.last(), Some(Tokens::Number(_))) {
                            symbols.push(Tokens::Multiply);
                        }
                        symbols.push(func);
                    } else {
                        for c in cs {
                            if matches!(symbols.last(), Some(token) if !token.is_operator()) {
                                symbols.push(Tokens::Multiply);
                            }
                            symbols.push(Tokens::Var(c));
                        }
                    }

                    continue;
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
            | Tokens::UnaryMinus
            | Tokens::Function(_) => true,
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SyntaxTree {
    value: Tokens,
    nodes: Vec<Self>,
}

impl SyntaxTree {
    fn get(tokens: Vec<Tokens>) -> Self {
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

    fn node(mut self, node: SyntaxTree) -> Self {
        self.nodes = vec![node];
        self
    }

    fn push_node(&mut self, node: SyntaxTree) {
        self.nodes.push(node);
    }

    fn pop_node(&mut self) {
        self.nodes.pop();
    }

    fn inline_vars(&mut self, vars: &HashMap<char, SyntaxTree>) {
        if let Tokens::Var(var) = self.value {
            if let Some(ast) = vars.get(&var) {
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
    tmp: Vec<SyntaxTree>,
}

impl<T: Iterator<Item = Tokens> + std::fmt::Debug> Parser<T> {
    // Grammar:
    // start      -> EXP | EXP = EXP
    // EXP        -> TERM1 T1
    // TERM1      -> TERM2 T2
    // TERM2      -> FUNCTION T3
    // T1         -> OP1 TERM1 T1 | epsilon
    // T2         -> OP2 TERM2 T2 | epsilon
    // T3         -> OP3 PARENS T3 | epsilon
    // FUNCTION   -> OPFUNC? PARENS
    // PARENS     -> OPBEUNARY? (number | ParenOpen EXP{, EXP}? ParenClose) OPAFUNARY?
    // OP1        -> + | -
    // OP2        -> * | / | %
    // OP3        -> ^
    // OPBEUNARY  -> + | -
    // OPAFUNARY  -> !
    // OPFUNC     -> functions eg. sqrt
    fn parse(tokens: Vec<Tokens>) -> SyntaxTree {
        let mut tokens = Parser {
            iterator: tokens.into_iter().peekable(),
            tmp: vec![],
        };
        let value = tokens.exp();
        let peek = tokens.peek();

        if peek == None {
            value
        } else if peek == Some(&Tokens::Equals) {
            SyntaxTree::new(tokens.next().unwrap()).nodes(vec![value, tokens.exp()])
        } else {
            panic!("Something went wrong got {peek:#?}")
        }
    }

    fn exp(&mut self) -> SyntaxTree {
        let peek = self.peek();

        if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Number(_) | Tokens::Var(_) | Tokens::ParenOpen | Tokens::Function(_)))
        {
            let term1 = self.term1();
            self.t1(term1)
        } else {
            panic!("Expected PrefixUnary or Number or Var or ParenOpen got {peek:#?}")
        }
    }

    fn term1(&mut self) -> SyntaxTree {
        let peek = self.peek();

        if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Number(_) | Tokens::Var(_) | Tokens::ParenOpen | Tokens::Function(_)))
        {
            let term2 = self.term2();
            self.t2(term2)
        } else {
            panic!("Expected PrefixUnary or Number or Var or ParenOpen got {peek:#?}")
        }
    }

    fn term2(&mut self) -> SyntaxTree {
        let peek = self.peek();

        if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Number(_) | Tokens::Var(_) | Tokens::ParenOpen | Tokens::Function(_)))
        {
            let function = self.function();
            self.t3(function)
        } else {
            panic!("Expected PrefixUnary or Number or Var or ParenOpen got {peek:#?}")
        }
    }

    fn t1(&mut self, prev: SyntaxTree) -> SyntaxTree {
        if matches!(self.peek(), Some(token) if token.op_level() == 1) {
            let op = self.next().unwrap();
            let peek = self.peek();

            if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Number(_) | Tokens::Var(_) | Tokens::ParenOpen | Tokens::Function(_)))
            {
                let term1 = self.term1();
                self.t1(SyntaxTree::new(op).nodes(vec![prev, term1]))
            } else {
                panic!("Expected PrefixUnary or Number or Var or ParenOpen got {peek:#?}")
            }
        } else {
            prev
        }
    }

    fn t2(&mut self, prev: SyntaxTree) -> SyntaxTree {
        if matches!(self.peek(), Some(token) if token.op_level() == 2) {
            let op = self.next().unwrap();
            let peek = self.peek();

            if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Number(_) | Tokens::Var(_) | Tokens::ParenOpen | Tokens::Function(_)))
            {
                let term2 = self.term2();
                self.t2(SyntaxTree::new(op).nodes(vec![prev, term2]))
            } else {
                panic!("Expected PrefixUnary or Number or Var or ParenOpen got {peek:#?}")
            }
        } else {
            prev
        }
    }

    fn t3(&mut self, mut prev: SyntaxTree) -> SyntaxTree {
        if matches!(self.peek(), Some(token) if token.op_level() == 3) {
            let op = self.next().unwrap();
            let peek = self.peek();

            if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Number(_) | Tokens::Var(_) | Tokens::ParenOpen | Tokens::Function(_)))
            {
                if prev.value == Tokens::Power {
                    let node = prev.nodes.pop().unwrap();

                    prev.nodes.push(SyntaxTree::new(op).nodes(vec![
                        node,
                        self.parens().into_iter().next().unwrap(),
                    ]));

                    prev
                } else {
                    let mut v = vec![prev];
                    v.append(&mut self.parens());
                    self.t3(SyntaxTree::new(op).nodes(v))
                }
            } else {
                panic!("Expected PrefixUnary or Number or Var or ParenOpen got {peek:#?}")
            }
        } else {
            prev
        }
    }

    fn function(&mut self) -> SyntaxTree {
        let peek = self.peek();

        if matches!(peek, Some(Tokens::Function(_))) {
            SyntaxTree::new(self.next().unwrap()).nodes(self.parens())
        } else if matches!(peek, Some(token) if token.is_prefix_unary() || matches!(token, Tokens::ParenOpen | Tokens::Number(_) | Tokens::Var(_)))
        {
            self.parens().into_iter().next().unwrap()
        } else {
            panic!("Expected Function or PrefixUnary or Number or Var or ParenOpen got {peek:#?}",)
        }
    }

    fn parens(&mut self) -> Vec<SyntaxTree> {
        let peek = self.peek();

        let unary = if matches!(peek, Some(token) if token.is_prefix_unary()) {
            self.next()
        } else {
            None
        };
        let peek = self.peek();
        let mut token = if matches!(peek, Some(Tokens::Number(_) | Tokens::Var(_))) {
            vec![SyntaxTree::new(self.next().unwrap())]
        } else if matches!(peek, Some(Tokens::ParenOpen)) {
            self.next();
            let mut value = vec![self.exp()];

            while let Some(&Tokens::ArgsSeparator) = self.peek() {
                self.next();
                value.push(self.exp());
            }

            let peek = self.peek();

            if peek == Some(&Tokens::ParenClose) {
                self.next();
                value
            } else {
                panic!("Expected ParenOpen got {peek:#?}")
            }
        } else {
            panic!("Expected PrefixUnary or Number or Var or ParenOpen got {peek:#?}",)
        };

        if matches!(self.peek(), Some(token) if token.is_postfix_unary()) {
            token = vec![SyntaxTree::new(self.next().unwrap()).nodes(token)]
        }

        if let Some(unary) = unary {
            vec![SyntaxTree::new(unary).nodes(token)]
        } else {
            token
        }
    }

    fn peek(&mut self) -> Option<&Tokens> {
        self.iterator.peek()
    }

    fn next(&mut self) -> Option<Tokens> {
        self.iterator.next()
    }
}

pub struct Equation {
    input: String,
    ast: SyntaxTree,
    answer: Option<Vec<Tokens>>,
    vars: Option<HashMap<char, SyntaxTree>>,
    functions: Option<HashMap<String, (SyntaxTree, usize)>>,
}

impl Equation {
    pub fn new(
        equation: String,
        vars: Option<HashMap<char, SyntaxTree>>,
        functions: Option<HashMap<String, (SyntaxTree, usize)>>,
    ) -> Equation {
        Equation {
            ast: SyntaxTree::get(Tokens::tokenize_string(&equation, None)),
            input: equation,
            answer: None,
            vars,
            functions,
        }
    }

    pub fn test_tokens(input: String) {
        println!("{:#?}", Tokens::tokenize_string(&input, None));
    }

    pub fn solve(&mut self) {
        if let Some(vars) = &self.vars {
            self.ast.inline_vars(&vars);
        }
        println!("{:#?}", self.ast);
        println!("{}", self.ast.solve());
    }
}
