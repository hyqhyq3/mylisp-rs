// MyLisp 快速性能基准测试
// 用于快速验证和回归测试，减少样本数和测试规模

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use mylisp::ast::Expr;
use mylisp::env::Env;
use mylisp::eval::Evaluator;
use mylisp::lexer::Lexer;
use mylisp::parser::Parser;

fn eval_lisp(code: &str) -> Result<Expr, String> {
    let mut env = Env::new();
    let lexer = Lexer::new(code);
    let mut parser = Parser::new(lexer);
    match parser.parse() {
        Ok(expr) => Evaluator::eval(expr, &mut env),
        Err(e) => Err(e),
    }
}

fn bench_quick_fib(c: &mut Criterion) {
    let mut group = c.benchmark_group("quick");
    group.sample_size(10); // 减少样本数

    for n in [10, 12, 15].iter() {
        group.bench_with_input(BenchmarkId::new("fib", n), n, |b, &n| {
            let code = format!(
                "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib {})",
                n
            );
            b.iter(|| eval_lisp(black_box(&code)))
        });
    }
    group.finish();
}

fn bench_quick_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("quick");
    group.sample_size(10);

    // 基本列表操作
    group.bench_function("list_create_50", |b| {
        let code = "(define (range s e) (if (>= s e) () (cons s (range (+ s 1) e)))) (range 1 51)";
        b.iter(|| eval_lisp(black_box(code)))
    });

    // 简单递归
    group.bench_function("sum_1_to_50", |b| {
        let code = "(define (sum n) (if (= n 0) 0 (+ n (sum (- n 1))))) (sum 50)";
        b.iter(|| eval_lisp(black_box(code)))
    });

    // 闭包调用
    group.bench_function("closure_100_calls", |b| {
        let code = "(define (make-adder n) (lambda (x) (+ x n))) (define add5 (make-adder 5)) (define (apply-n f n a) (if (= n 0) a (apply-n f (- n 1) (f a)))) (apply-n add5 100 0)";
        b.iter(|| eval_lisp(black_box(code)))
    });

    group.finish();
}

criterion_group!(benches, bench_quick_fib, bench_quick_operations);
criterion_main!(benches);
