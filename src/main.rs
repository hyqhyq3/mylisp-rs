// 导入库模块
use mylisp::ast::Expr;
use mylisp::env::Env;
use mylisp::eval::Evaluator;
use mylisp::lexer::Lexer;
use mylisp::parser::Parser;
use std::io::{self, Write};

fn print_prompt(prompt: &str) {
    print!("{}", prompt);
    io::stdout().flush().unwrap();
}

fn read_line() -> Option<String> {
    let mut input = String::new();
    match io::stdin().read_line(&mut input) {
        Ok(0) => None, // EOF
        Ok(_) => Some(input.trim().to_string()),
        Err(_) => None,
    }
}

fn repl() {
    println!("MyLisp v0.1.0");
    println!("Type 'exit' or 'quit' to exit");
    println!();

    let mut env = Env::new();

    // Add some built-in functions
    loop {
        print_prompt("> ");
        let input = match read_line() {
            Some(line) => line,
            None => {
                println!(); // newline after EOF
                break;
            }
        };

        if input == "exit" || input == "quit" {
            println!("Goodbye!");
            break;
        }

        if input.is_empty() {
            continue;
        }

        match eval_str(&input, &mut env) {
            Ok(result) => println!("{}", result),
            Err(e) => eprintln!("Error: {}", e),
        }
    }
}

fn eval_str(input: &str, env: &mut Env) -> Result<Expr, String> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    match parser.parse() {
        Ok(expr) => Evaluator::eval(expr, env),
        Err(e) => Err(e),
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 1 {
        // Run file
        let filename = &args[1];
        match std::fs::read_to_string(filename) {
            Ok(content) => {
                let mut env = Env::new();
                // 解析整个文件内容，支持跨行表达式
                if let Err(e) = eval_program(&content, &mut env) {
                    eprintln!("Error: {}", e);
                }
            }
            Err(e) => eprintln!("Error reading file {}: {}", filename, e),
        }
    } else {
        // REPL mode
        repl();
    }
}

fn eval_program(input: &str, env: &mut Env) -> Result<(), String> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    loop {
        match parser.parse() {
            Ok(expr) => {
                match Evaluator::eval(expr, env) {
                    Ok(_) => {},
                    Err(e) => eprintln!("Error: {}", e),
                }
            }
            Err(e) => {
                // 如果是 EOF 错误，说明正常结束
                if e.contains("EOF") {
                    break;
                }
                return Err(e);
            }
        }
    }

    Ok(())
}
