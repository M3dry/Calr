use calr::*;

fn main() {
    // let mut equation = Equation::new("(1 + 12 - 123 * (1234 / 12345))".to_string());
    // equation.solve();
    // let mut equation = Equation::new("(1 + 1 + (2 + 3) / 5) + (4 / 4)".to_string());
    // equation.solve();
    // let mut equation = Equation::new("3 + 4 * 2 / (1 - 5) ^ 2 ^ 3".to_string());
    // equation.solve();
    let mut equation = Equation::new("-(5*2) = -(1 + 9)".to_string());
    equation.solve();
}
