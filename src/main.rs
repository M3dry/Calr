use calr::{parser::ast, Repl};

fn main() {
    let args = std::env::args().collect::<Vec<String>>();

    if args.len() > 2 && args[1] == "-t" {
        Repl::test_tokens(args[2].to_string())
    } else if args.len() > 2 && args[1] == "-s" {
        Repl::test_ast(args[2].to_string())
    } else {
        let mut repl = Repl::new(ast::Vars::none(), ast::Functions::none());
        for i in 1..args.len() {
            print!("{:?} ", repl.input(args[i].to_string()));
            repl.test_last_ast();
        }
    }
}
