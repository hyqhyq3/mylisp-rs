// MyLisp 火焰图生成
// 使用 pprof/criterion 生成性能火焰图

use criterion::{black_box, Criterion, BenchmarkId};
use mylisp::ast::Expr;
use mylisp::env::Env;
use mylisp::eval::Evaluator;
use mylisp::lexer::Lexer;
use mylisp::parser::Parser;
use pprof::criterion::{PProfProfiler, Output};

/// 执行 Lisp 代码的辅助函数
fn eval_lisp(code: &str) -> Result<Expr, String> {
    let mut env = Env::new();
    let lexer = Lexer::new(code);
    let mut parser = Parser::new(lexer);

    match parser.parse() {
        Ok(expr) => Evaluator::eval(expr, &mut env),
        Err(e) => Err(e),
    }
}

fn main() {
    let mut criterion = Criterion::default()
        .with_profiler(PProfProfiler::new(100, Output::Flamegraph(Some("target/flamegraph"))));

    // 斐波那契火焰图
    criterion.bench_function("flamegraph_fib_25", |b| {
        let code = r#"
            (define (fib n)
              (if (< n 2)
                  n
                  (+ (fib (- n 1)) (fib (- n 2)))))
            (fib 25)
        "#;

        b.iter(|| eval_lisp(black_box(code)))
    });

    // 列表操作火焰图
    criterion.bench_function("flamegraph_map_reduce_100", |b| {
        let code = r#"
            (define (map fn lst)
              (if (null? lst)
                  ()
                  (cons (fn (car lst)) (map fn (cdr lst)))))

            (define (reduce fn init lst)
              (if (null? lst)
                  init
                  (reduce fn (fn init (car lst)) (cdr lst))))

            (define (range start end)
              (if (>= start end)
                  ()
                  (cons start (range (+ start 1) end))))

            (define (square x) (* x x))
            (reduce + 0 (map square (range 1 101)))
        "#;

        b.iter(|| eval_lisp(black_box(code)))
    });

    // 闭包火焰图
    criterion.bench_function("flamegraph_closure_1000", |b| {
        let code = r#"
            (define (make-adder n)
              (lambda (x) (+ x n)))

            (define add5 (make-adder 5))

            (define (apply-n fn n arg)
              (if (= n 0)
                  arg
                  (apply-n fn (- n 1) (fn arg))))

            (apply-n add5 1000 0)
        "#;

        b.iter(|| eval_lisp(black_box(code)))
    });

    // 尾调用优化火焰图
    criterion.bench_function("flamegraph_tco_10000", |b| {
        let code = r#"
            (define (sum-tail n acc)
              (if (= n 0)
                  acc
                  (sum-tail (- n 1) (+ acc n))))
            (sum-tail 10000 0)
        "#;

        b.iter(|| eval_lisp(black_box(code)))
    });

    criterion.final_summary();
}
