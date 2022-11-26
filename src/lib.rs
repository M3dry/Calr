use std::iter::Peekable;

#[derive(Debug, PartialEq, Eq)]
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
    ParenOpen,
    ParenClose,
    Equals,
    Identifier(Vec<(u128, Vec<char>)>),
}

impl Tokens {
    fn tokenize_string(str: &String) -> Vec<Self> {
        let mut symbols: Vec<Tokens> = vec![];
        let mut chars = str.chars().into_iter().peekable();

        while let Some(char) = chars.next() {
            symbols.push(match char {
                '+' if (matches!(symbols.last(), Some(symbol) if symbol.is_operator())
                    || symbols.last() == None) =>
                {
                    Tokens::UnaryPlus
                }
                '+' => Tokens::Plus,
                '-' if (matches!(symbols.last(), Some(symbol) if symbol.is_operator())
                    || symbols.last() == None) =>
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
                c if c.is_ascii_alphanumeric() => {
                    let mut identifier: Vec<(u128, Vec<char>)> = vec![];
                    let mut cur: (u128, Vec<char>) = if c.is_digit(10) {
                        (c.to_digit(10).unwrap() as u128, vec![])
                    } else {
                        (0, vec![c])
                    };

                    while let Some(c) = chars.peek() {
                        if c.is_ascii_digit() {
                            if !cur.1.is_empty() {
                                identifier.push(cur);
                                cur = (chars.next().unwrap().to_digit(10).unwrap() as u128, vec![]);
                            } else {
                                cur.0 = cur.0 * 10 + chars.next().unwrap().to_digit(10).unwrap() as u128;
                            }
                        } else if c.is_ascii_alphabetic() {
                            cur.1.push(chars.next().unwrap())
                        } else {
                            break;
                        }
                    }

                    identifier.push(cur);
                    Tokens::Identifier(identifier)
                }
                _ => continue,
            })
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
            Tokens::Power | Tokens::Factorial => 3,
            Tokens::Multiply | Tokens::Divide | Tokens::Modulo => 2,
            Tokens::Plus | Tokens::Minus => 1,
            _ => 0,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct SyntaxTree {
    value: Tokens,
    left: Option<Box<SyntaxTree>>,
    right: Option<Box<SyntaxTree>>,
}

impl SyntaxTree {
    fn parse(tokens: Vec<Tokens>) -> Self {
        Parser::<std::vec::IntoIter<Tokens>>::parse(tokens)
    }

    fn new(value: Tokens) -> Self {
        SyntaxTree {
            value,
            left: None,
            right: None,
        }
    }

    fn left(mut self, left: Option<SyntaxTree>) -> Self {
        self.left = if let Some(left) = left {
            Some(Box::new(left))
        } else {
            None
        };
        self
    }

    fn right(mut self, right: Option<SyntaxTree>) -> Self {
        self.right = if let Some(right) = right {
            Some(Box::new(right))
        } else {
            None
        };
        self
    }
}

struct Parser<T: Iterator<Item = Tokens>> {
    iterator: Peekable<T>,
}

impl<T: Iterator<Item = Tokens>> Parser<T> {
    // Grammar:
    // start      -> EXP | EXP = EXP
    // EXP        -> TERM1 T1
    // TERM1      -> TERM2 T2
    // TERM2      -> PREUNARY T3
    // T1         -> OP1 TERM1 T1 | epsilon
    // T2         -> OP2 TERM2 T2 | epsilon
    // T3         -> OP3 PREUNARY T3 | epsilon
    // PREUNARY   -> PARENS | OPBEUNARY PARENS
    // PARENS     -> (number | ParenOpen EXP ParenClose) OPAFUNARY?
    // OP1        -> + | -
    // OP2        -> * | / | %
    // OP3        -> ^
    // OPBEUNARY -> + | -
    // OPAFUNARY -> !
    fn parse(tokens: Vec<Tokens>) -> SyntaxTree {
        let mut tokens = Parser {
            iterator: tokens.into_iter().peekable(),
        };
        let value = tokens.exp();

        if tokens.iterator.peek() == None {
            value
        } else if tokens.iterator.peek() == Some(&Tokens::Equals) {
            SyntaxTree::new(tokens.iterator.next().unwrap())
                .left(Some(value))
                .right(Some(tokens.exp()))
        } else {
            panic!("Shit bad")
        }
    }

    fn exp(&mut self) -> SyntaxTree {
        if matches!(self.iterator.peek(), Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Identifier(_) | Tokens::ParenOpen))
        {
            let term1 = self.term1();
            self.t1(term1)
        } else {
            panic!("Expected PrefixUnary or Identifier or ParenOpen")
        }
    }

    fn term1(&mut self) -> SyntaxTree {
        if matches!(self.iterator.peek(), Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Identifier(_) | Tokens::ParenOpen))
        {
            let term2 = self.term2();
            self.t2(term2)
        } else {
            panic!("Expected PrefixUnary or Identifier or ParenOpen")
        }
    }

    fn term2(&mut self) -> SyntaxTree {
        if matches!(self.iterator.peek(), Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Identifier(_) | Tokens::ParenOpen))
        {
            let pre_unary = self.pre_unary();
            self.t3(pre_unary)
        } else {
            panic!("Expected PrefixUnary or Identifier or ParenOpen")
        }
    }

    fn t1(&mut self, prev: SyntaxTree) -> SyntaxTree {
        if matches!(self.iterator.peek(), Some(token) if token.op_level() == 1) {
            let op = self.iterator.next().unwrap();
            if matches!(self.iterator.peek(), Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Identifier(_) | Tokens::ParenOpen))
            {
                let term1 = self.term1();
                self.t1(SyntaxTree::new(op).left(Some(prev)).right(Some(term1)))
            } else {
                panic!("Expected PrefixUnary or Identifier or ParenOpen")
            }
        } else {
            prev
        }
    }

    fn t2(&mut self, prev: SyntaxTree) -> SyntaxTree {
        if matches!(self.iterator.peek(), Some(token) if token.op_level() == 2) {
            let op = self.iterator.next().unwrap();
            if matches!(self.iterator.peek(), Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Identifier(_) | Tokens::ParenOpen))
            {
                let term2 = self.term2();
                self.t2(SyntaxTree::new(op).left(Some(prev)).right(Some(term2)))
            } else {
                panic!("Expected PrefixUnary or Identifier or ParenOpen")
            }
        } else {
            prev
        }
    }

    fn t3(&mut self, prev: SyntaxTree) -> SyntaxTree {
        if matches!(self.iterator.peek(), Some(token) if token.op_level() == 3) {
            let op = self.iterator.next().unwrap();
            if matches!(self.iterator.peek(), Some(token) if token.is_prefix_unary() || matches!(token, Tokens::Identifier(_) | Tokens::ParenOpen))
            {
                let pre_unary = self.pre_unary();
                self.t3(SyntaxTree::new(op).left(Some(prev)).right(Some(pre_unary)))
            } else {
                panic!("Expected PrefixUnary or Identifier or ParenOpen")
            }
        } else {
            prev
        }
    }

    fn pre_unary(&mut self) -> SyntaxTree {
        if matches!(
            self.iterator.peek(),
            Some(Tokens::Identifier(_) | Tokens::ParenOpen)
        ) {
            self.parens()
        } else if matches!(self.iterator.peek(), Some(token) if token.is_prefix_unary()) {
            SyntaxTree::new(self.iterator.next().unwrap()).left(Some(self.parens()))
        } else {
            panic!("Expected PrefixUnary or Identifier or ParenOpen")
        }
    }

    fn parens(&mut self) -> SyntaxTree {
        let token = if matches!(self.iterator.peek(), Some(Tokens::Identifier(_))) {
            SyntaxTree::new(self.iterator.next().unwrap())
        } else if matches!(self.iterator.peek(), Some(Tokens::ParenOpen)) {
            self.iterator.next();
            let value = self.exp();
            if self.iterator.next() == Some(Tokens::ParenClose) {
                value
            } else {
                panic!("Expected ParenOpen")
            }
        } else {
            panic!("Expected Identifier or ParenOpen")
        };

        if matches!(self.iterator.peek(), Some(token) if token.is_postfix_unary()) {
            SyntaxTree::new(self.iterator.next().unwrap()).left(Some(token))
        } else {
            token
        }
    }
}

pub struct Equation {
    input: String,
    ast: SyntaxTree,
    answer: Option<Vec<Tokens>>,
}

impl Equation {
    pub fn new(equation: String) -> Equation {
        Equation {
            ast: SyntaxTree::parse(Tokens::tokenize_string(&equation)),
            input: equation,
            answer: None,
        }
    }

    pub fn test_tokens(input: String) {
        println!("{:#?}", Tokens::tokenize_string(&input));
    }

    pub fn solve(&mut self) {
        println!("{:#?}", self.ast);
    }
}