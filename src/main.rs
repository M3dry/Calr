use calr::*;

fn main() {

    Equation::new(std::env::args().collect::<Vec<String>>().into_iter().nth(1).unwrap(), None, None).solve()
    // Equation::test_tokens(std::env::args().collect::<Vec<String>>().into_iter().nth(1).unwrap())
}
