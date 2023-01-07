use calr::*;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();

    if args.len() > 2 && args[1] == "-t" {
        Repl::test_tokens(args[2].to_string())
    } else {
        let mut repl = Repl::new(None, None);
        println!("{:?}", repl.input(args[1].to_string()));
        repl.test_last_ast();
    }
}
