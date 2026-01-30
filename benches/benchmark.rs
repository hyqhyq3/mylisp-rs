// MyLisp 性能基准测试
// 使用 Criterion.rs 进行性能测量

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use mylisp::ast::Expr;
use mylisp::env::Env;
use mylisp::eval::Evaluator;
use mylisp::lexer::Lexer;
use mylisp::parser::Parser;

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

/// 执行整个 Lisp 程序（多表达式）
fn eval_program(code: &str) -> Result<(), String> {
    let mut env = Env::new();
    let lexer = Lexer::new(code);
    let mut parser = Parser::new(lexer);

    loop {
        match parser.parse() {
            Ok(expr) => {
                Evaluator::eval(expr, &mut env)?;
            }
            Err(e) => {
                if e.contains("EOF") {
                    break;
                }
                return Err(e);
            }
        }
    }

    Ok(())
}

/// 加载并执行测试文件
fn load_and_execute(filename: &str) -> Result<Expr, String> {
    let code = std::fs::read_to_string(filename)
        .map_err(|e| format!("Failed to read file: {}", e))?;

    let mut env = Env::new();
    let lexer = Lexer::new(&code);
    let mut parser = Parser::new(lexer);

    let mut result = Expr::Nil;

    loop {
        match parser.parse() {
            Ok(expr) => {
                result = Evaluator::eval(expr, &mut env)?;
            }
            Err(e) => {
                if e.contains("EOF") {
                    break;
                }
                return Err(e);
            }
        }
    }

    Ok(result)
}

// ==============================================================================
// 基准测试函数
// ==============================================================================

/// 斐波那契数列基准测试 - 测试递归和尾调用优化性能
fn bench_fibonacci(c: &mut Criterion) {
    let mut group = c.benchmark_group("fibonacci");

    // 减少递归规模以提高测试速度
    for n in [10, 15, 18].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let code = format!(
                r#"
                (define (fib n)
                  (if (< n 2)
                      n
                      (+ (fib (- n 1)) (fib (- n 2)))))
                (fib {})
                "#,
                n
            );

            b.iter(|| eval_lisp(black_box(&code)))
        });
    }

    group.finish();
}

/// 算术运算基准测试
fn bench_arithmetic(c: &mut Criterion) {
    c.bench_function("arithmetic_sum_1_to_100", |b| {
        let code = r#"
            (define (sum n)
              (if (= n 0)
                  0
                  (+ n (sum (- n 1)))))
            (sum 100)
        "#;

        b.iter(|| eval_lisp(black_box(code)))
    });

    c.bench_function("arithmetic_factorial_10", |b| {
        let code = r#"
            (define (fact n)
              (if (< n 2)
                  1
                  (* n (fact (- n 1)))))
            (fact 10)
        "#;

        b.iter(|| eval_lisp(black_box(code)))
    });
}

/// 列表操作基准测试
fn bench_list_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("list_operations");

    group.bench_function("create_list_100", |b| {
        let code = r#"
            (define (range start end)
              (if (>= start end)
                  ()
                  (cons start (range (+ start 1) end))))
            (range 1 101)
        "#;

        b.iter(|| eval_lisp(black_box(code)))
    });

    group.bench_function("map_square_100", |b| {
        let code = r#"
            (define (map fn lst)
              (if (null? lst)
                  ()
                  (cons (fn (car lst)) (map fn (cdr lst)))))

            (define (range start end)
              (if (>= start end)
                  ()
                  (cons start (range (+ start 1) end))))

            (define (square x) (* x x))
            (map square (range 1 101))
        "#;

        b.iter(|| eval_lisp(black_box(code)))
    });

    group.bench_function("reduce_sum_100", |b| {
        let code = r#"
            (define (reduce fn init lst)
              (if (null? lst)
                  init
                  (reduce fn (fn init (car lst)) (cdr lst))))

            (define (range start end)
              (if (>= start end)
                  ()
                  (cons start (range (+ start 1) end))))

            (reduce + 0 (range 1 101))
        "#;

        b.iter(|| eval_lisp(black_box(code)))
    });

    group.finish();
}

/// Lambda 闭包基准测试
fn bench_closures(c: &mut Criterion) {
    c.bench_function("closure_make_adder_1000_calls", |b| {
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

    c.bench_function("closure_y_combinator_fact_10", |b| {
        let code = r#"
            (define (Y fn)
              ((lambda (x) (x x))
               (lambda (x) (fn (lambda (v) ((x x) v))))))

            (define fact
              (Y (lambda (f)
                   (lambda (n)
                     (if (< n 2)
                         1
                         (* n (f (- n 1))))))))

            (fact 10)
        "#;

        b.iter(|| eval_lisp(black_box(code)))
    });
}

/// 宏展开基准测试
fn bench_macros(c: &mut Criterion) {
    c.bench_function("macro_when_10_times", |b| {
        let code = r#"
            (define-syntax when
              (syntax-rules ()
                ((when test body1 body2 ...)
                 (if test
                     (begin body1 body2 ...)))))

            (define (test-when n)
              (if (= n 0)
                  0
                  (begin
                    (when #t 0)
                    (test-when (- n 1)))))

            (test-when 10)
        "#;

        b.iter(|| eval_program(black_box(code)))
    });
}

/// 文件加载基准测试（仅测量解析时间，不执行）
fn bench_file_loading(c: &mut Criterion) {
    let mut group = c.benchmark_group("file_loading");

    // 减少样本数，因为文件解析相对较慢
    group.sample_size(20);

    // 测试各个测试文件的加载性能（仅解析，不执行）
    let test_files = [
        ("fib", "benches/data/fib.lisp"),
        ("primes", "benches/data/primes.lisp"),
        ("list_ops", "benches/data/list_ops.lisp"),
        ("closure", "benches/data/closure.lisp"),
        ("macros", "benches/data/macros.lisp"),
    ];

    for (name, path) in test_files.iter() {
        // 确保文件存在
        if std::path::Path::new(path).exists() {
            group.bench_with_input(BenchmarkId::from_parameter(name), path, |b, path| {
                b.iter(|| {
                    let code = std::fs::read_to_string(path).unwrap();
                    let lexer = Lexer::new(&code);
                    let mut parser = Parser::new(lexer);
                    // 仅解析，不求值
                    while parser.parse().is_ok() {}
                })
            });
        }
    }

    group.finish();
}

/// 尾调用优化基准测试
fn bench_tco(c: &mut Criterion) {
    let mut group = c.benchmark_group("tail_call_optimization");

    // 减少规模以加快测试速度
    group.bench_function("tco_sum_1000", |b| {
        let code = r#"
            (define (sum-tail n acc)
              (if (= n 0)
                  acc
                  (sum-tail (- n 1) (+ acc n))))
            (sum-tail 1000 0)
        "#;

        b.iter(|| eval_lisp(black_box(code)))
    });

    group.bench_function("tco_factorial_100", |b| {
        let code = r#"
            (define (fact-tail n acc)
              (if (< n 2)
                  acc
                  (fact-tail (- n 1) (* acc n))))
            (fact-tail 100 1)
        "#;

        b.iter(|| eval_lisp(black_box(code)))
    });

    group.finish();
}

/// 惰性求值基准测试
fn bench_lazy_evaluation(c: &mut Criterion) {
    c.bench_function("lazy_thunk_creation_100", |b| {
        let code = r#"
            (define (delay expr)
              (lambda () expr))

            (define (force thunk)
              (thunk))

            (define (create-thunks n)
              (if (= n 0)
                  ()
                  (cons (delay n) (create-thunks (- n 1)))))

            (define (sum-thunks thunks)
              (if (null? thunks)
                  0
                  (+ (force (car thunks)) (sum-thunks (cdr thunks)))))

            (sum-thunks (create-thunks 100))
        "#;

        b.iter(|| eval_lisp(black_box(code)))
    });
}

criterion_group!(
    benches,
    bench_fibonacci,
    bench_arithmetic,
    bench_list_operations,
    bench_closures,
    bench_macros,
    bench_file_loading,
    bench_tco,
    bench_lazy_evaluation
);
criterion_main!(benches);
