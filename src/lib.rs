mod parser;
mod tokenizer;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;

    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }

    #[test]
    fn test_number() {
        let num = " 11 ";
        let mut p = Parser::new();
        let _ast = p.parse(num);
        // assert_eq!(ast, Number(11 as f64))
    }

    #[test]
    fn test_unexpected() {
        let str = "\"hello\"";
        let mut p = Parser::new();
        let _ast = p.parse(str);
        // assert_eq!(ast, StringLiteral(String::from("hello")))
    }

    #[test]
    fn test_white_space() {
        let str = " \"hello\"      ";
        let mut p = Parser::new();
        let _ast = p.parse(str);
        // assert_eq!(ast, StringLiteral(String::from("hello")))
    }

    #[test]
    fn test_skip_single_line_comments() {
        let str = " \
        // String
        \"hello\"      \
        ";
        let mut p = Parser::new();
        let _ast = p.parse(str);
        // assert_eq!(ast, StringLiteral(String::from("hello")))
    }

    #[test]
    fn test_skip_multiple_line_comments() {
        let str = " \
        /**
         *  123
         */
        \"hello\"      \
        ";
        let mut p = Parser::new();
        let _ast = p.parse(str);
        // assert_eq!(ast, StringLiteral(String::from("hello")))
    }

    #[test]
    fn it_worked() {
        let program = "\"hello\";  \
           42;                \
           \"world\";                     \
        ";

        let ast = Parser::new().parse(program);
        println!("{:?}", ast)
    }
}
