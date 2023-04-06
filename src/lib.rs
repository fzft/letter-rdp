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
        let num = " 11;";
        let mut p = Parser::new();
        let _ast = p.parse(num);
        // assert_eq!(ast, Number(11 as f64))
    }

    #[test]
    fn test_unexpected() {
        let str = "\"hello\";";
        let mut p = Parser::new();
        let _ast = p.parse(str);
        // assert_eq!(ast, StringLiteral(String::from("hello")))
    }

    #[test]
    fn test_white_space() {
        let str = " \"hello\";      ";
        let mut p = Parser::new();
        let _ast = p.parse(str);
        // assert_eq!(ast, StringLiteral(String::from("hello")))
    }

    #[test]
    fn test_skip_single_line_comments() {
        let str = " \
        // String
        \"hello\";      \
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
        \"hello\";      \
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

        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn omit_semicolon() {
        let program = "\"hello\"  \
           42;                \
           \"world\";                     \
        ";
        let ast = Parser::new().parse(program);
        assert!(ast.is_err(), "expect a semicolon")
    }

    #[test]
    fn with_block() {
        let program = "     \
          {               \
           42;         \
                          \
           \"hello\";          \
               }     \
        ";
        let ast = Parser::new().parse(program);
        println!("{:?}", ast)
    }

    #[test]
    fn test_plus() {
        let program = "2+2;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn test_plus_mul() {
        let program = "2+2*3;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn test_plus_mul_with_paren() {
        let program = " (2+2)*3;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn test_plus_mul_with_rare_paten() {
        let program = " 3*(2+1);";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn simple_assign() {
        let program = " x=42 ;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn chain_assign() {
        let program = " x= y = 42 ;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn assign2() {
        let program = " x+x ;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn assign3() {
        let program = " x=y+10 ;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }


    #[test]
    fn assign4() {
        let program = " x += 10;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }
    #[test]
    fn assign5() {
        let program = " x = 10 / 2;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }
    #[test]
    fn div_equal_assign() {
        let program = " x /= 10 ;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn var_assign1() {
        let program = " let x, y ;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn var_assign2() {
        let program = " let x = 42;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn var_assign3() {
        let program = " let x;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn var_assign4() {
        let program = " let x, y = 42;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn var_assign5() {
        let program = " let x = y = 42;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn if_stmt1() {
        let program = " if (x) { \
        x = 1;\
        } else {\
        x=2;\
        }";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn if_stmt2() {
        let program = " if (x) { \
        x = 1;\
        } ";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn if_stmt3() {
        let program = " if (x< 10) { \
        x = 1;\
        } ";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }
    #[test]
    fn if_stmt4() {
        let program = " if (x + 5 > 10) { \
        x = 1;\
        } ";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn if_stmt5() {
        let program = " if (y = x + 5 > 10) { \
        x = 1;\
        } ";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn binary_assign() {
        let program = "x <5;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn equality1() {
        let program = "x + 5 > 10 == true;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }
    #[test]
    fn equality2() {
        let program = "x + 5 > 10 == false;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }
    #[test]
    fn equality3() {
        let program = "x + 5 > 10 == null;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn logical1() {
        let program = "x > 0 && y < 0;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn logical2() {
        let program = "x > 0 || y < 0;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn unary1() {
        let program = "!x; -x;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn unary2() {
        let program = "--x;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }

    #[test]
    fn loops() {
        let program = "--x;";
        let ast = Parser::new().parse(program).unwrap();
        println!("{:?}", ast)
    }






}
