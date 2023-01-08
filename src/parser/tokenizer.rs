use std::collections::{HashMap, HashSet};

use crate::SyntaxTree;

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum Functions {
    Sqrt,
    Cbrt,
    Root,
    Log,
    Ln,
    Custom(String),
}

impl Functions {
    pub(crate) fn get_args(&self) -> usize {
        match self {
            Functions::Root | Functions::Log => 2,
            Functions::Sqrt | Functions::Cbrt | Functions::Ln => 1,
            Functions::Custom(_) => 0,
        }
    }

    pub(crate) fn from_string(str: &str) -> Option<Self> {
        match str {
            "sqrt" => Some(Functions::Sqrt),
            "cbrt" => Some(Functions::Cbrt),
            "root" => Some(Functions::Root),
            "log" => Some(Functions::Log),
            "ln" => Some(Functions::Ln),
            _ => None,
        }
    }

    pub(crate) fn get_all() -> Vec<&'static str> {
        vec!["sqrt", "cbrt", "root", "log", "ln"]
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum Tokens {
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
    VarName(String),
    FuncName(String),
    FuncArgs(usize),
    FuncArg(usize),
}

impl Tokens {
    pub(crate) fn tokenize_string(
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

            ret.append(&mut Self::tokenize_function_body(body, functions, args));

            Ok(ret)
        } else if str.len() > 8 && &str[..8] == "variable" {
            let (name, body) = str[9..].split_once(' ').ok_or("Can't find name")?;
            let mut ret = vec![Self::VarName(name.to_string())];

            ret.append(&mut Self::tokenize_equation(body, functions, vars));

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
                        vars: Option<&HashMap<String, SyntaxTree>>,
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
                                    token =
                                        Some(Tokens::Function(Functions::Custom(key.to_string())));
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
                                continue;
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
                                    token =
                                        Some(Tokens::Function(Functions::Custom(key.to_string())));
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
                                continue;
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

    pub(crate) fn is_prefix_unary(&self) -> bool {
        match self {
            Tokens::UnaryPlus | Tokens::UnaryMinus => true,
            _ => false,
        }
    }

    pub(crate) fn is_postfix_unary(&self) -> bool {
        match self {
            Tokens::Factorial => true,
            _ => false,
        }
    }

    pub(crate) fn op_level(&self) -> usize {
        match self {
            Tokens::Function(_) => 4,
            Tokens::Power | Tokens::Factorial => 3,
            Tokens::Multiply | Tokens::Divide | Tokens::Modulo => 2,
            Tokens::Plus | Tokens::Minus => 1,
            _ => 0,
        }
    }
}
