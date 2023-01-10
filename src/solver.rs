use crate::parser::{ast::SyntaxTree, tokenizer::Tokens};

#[derive(Debug)]
pub(crate) struct Solver(pub(crate) SyntaxTree);

impl Solver {
    /// multiplies a fraction by another fraction or a value
    /// eg. 5/2 * 2/10; 5 * 2/10; 2/10 * 5
    pub(crate) fn multiply_fraction(mut self) -> Self {
        if self.0.value != Tokens::Multiply {
            panic!("multiply_fraction: Expected multiply got {self:#?}")
        }

        let (mut divide, mut value, both) = match (&self.0.nodes[0].value, &self.0.nodes[1].value) {
            (Tokens::Divide, Tokens::Divide) => (
                self.0.nodes.pop().unwrap(),
                self.0.nodes.pop().unwrap(),
                true,
            ),
            (_, Tokens::Divide) => (
                self.0.nodes.pop().unwrap(),
                self.0.nodes.pop().unwrap(),
                false,
            ),
            (Tokens::Divide, _) => {
                let value = self.0.nodes.pop().unwrap();
                (self.0.nodes.pop().unwrap(), value, false)
            }
            _ => {
                unreachable!("multiply_fraction: Expected Divide + Divide|Divide + _|_ | Divide got {self:#?}")
            }
        };

        if both {
            let bottom_div = divide.nodes.pop().unwrap();
            let bottom_val = value.nodes.pop().unwrap();
            let top_div = divide.nodes.pop().unwrap();
            let top_val = value.nodes.pop().unwrap();

            divide.nodes = vec![
                SyntaxTree::new(Tokens::Multiply).nodes(vec![top_val, top_div]),
                SyntaxTree::new(Tokens::Multiply).nodes(vec![bottom_val, bottom_div]),
            ];

            Self(divide)
        } else {
            println!("{divide:#?}\n{value:#?}");
            divide.nodes[0] = SyntaxTree::new(Tokens::Multiply)
                .nodes(vec![std::mem::replace(&mut divide.nodes[0], self.0), value]);

            Self(divide)
        }
    }

    fn shorten(self) -> Self {
        if self.0.value != Tokens::Divide {
            panic!("shorten: Expected Divide got {self:#?}")
        }
        self
    }
}
