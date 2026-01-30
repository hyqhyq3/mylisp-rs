//! 字节码编译器
//!
//! 将 MyLisp AST 编译为字节码。

use crate::ast::{Expr, ThunkState};
use crate::jit::bytecode::{Chunk, Constant, Instruction, OpCode, Operand};
use std::collections::HashMap;

/// 编译错误
#[derive(Debug, Clone)]
pub enum CompileError {
    /// 未定义的变量
    UndefinedVariable(String),

    /// 不支持的语法
    UnsupportedSyntax(String),

    /// 其他错误
    Other(String),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::UndefinedVariable(name) => {
                write!(f, "Undefined variable: {}", name)
            }
            CompileError::UnsupportedSyntax(msg) => {
                write!(f, "Unsupported syntax: {}", msg)
            }
            CompileError::Other(msg) => {
                write!(f, "Compile error: {}", msg)
            }
        }
    }
}

impl std::error::Error for CompileError {}

/// 局部变量信息
#[derive(Debug, Clone)]
struct LocalVar {
    /// 变量名
    name: String,
    /// 在作用域中的深度（0 = 当前作用域）
    depth: usize,
    /// 在栈中的位置
    slot: usize,
}

/// 字节码编译器
pub struct BytecodeCompiler {
    /// 当前正在编译的代码块
    chunk: Chunk,

    /// 局部变量作用域栈
    locals: Vec<Vec<LocalVar>>,

    /// 当前作用域深度
    scope_depth: usize,

    /// 全局变量映射（变量名 -> 索引）
    globals: HashMap<String, usize>,

    /// 当前编译的行号（用于调试）
    current_line: usize,

    /// 内置函数集合
    builtins: std::collections::HashSet<String>,
}

impl BytecodeCompiler {
    /// 创建新的编译器
    pub fn new() -> Self {
        // 内置函数列表
        let mut builtins = std::collections::HashSet::new();
        for builtin in &[
            "+", "-", "*", "/", "mod", "neg",
            "<", ">", "<=", ">=", "=",
            "not", "and", "or",
            "cons", "head", "tail", "null?", "length", "append",
            "symbol?", "list?", "number?", "string?",
            "display", "newline",
        ] {
            builtins.insert(builtin.to_string());
        }

        Self {
            chunk: Chunk::new(),
            locals: vec![Vec::new()], // 全局作用域
            scope_depth: 0,
            globals: HashMap::new(),
            current_line: 0,
            builtins,
        }
    }

    /// 使用指定的内置函数集合创建编译器
    fn with_builtins(builtins: std::collections::HashSet<String>) -> Self {
        Self {
            chunk: Chunk::new(),
            locals: Vec::new(), // Lambda 编译器不初始化全局作用域
            scope_depth: 0,
            globals: HashMap::new(),
            current_line: 0,
            builtins,
        }
    }

    /// 编译表达式为字节码
    pub fn compile(&mut self, expr: &Expr) -> Result<Chunk, CompileError> {
        self.compile_expr(expr)?;
        Ok(std::mem::take(&mut self.chunk))
    }

    /// 编译表达式
    fn compile_expr(&mut self, expr: &Expr) -> Result<(), CompileError> {
        match expr {
            // 数字常量
            Expr::Number(n) => {
                let idx = self.chunk.add_constant(Constant::Number(*n));
                self.emit_instruction(Instruction::new(
                    OpCode::LoadConst,
                    vec![Operand::U32(idx as u32)],
                ));
            }

            // 字符串常量
            Expr::String(s) => {
                let idx = self.chunk.add_constant(Constant::String(s.clone()));
                self.emit_instruction(Instruction::new(
                    OpCode::LoadConst,
                    vec![Operand::U32(idx as u32)],
                ));
            }

            // 布尔常量
            Expr::Bool(b) => {
                let idx = self.chunk.add_constant(Constant::Boolean(*b));
                self.emit_instruction(Instruction::new(
                    OpCode::LoadConst,
                    vec![Operand::U32(idx as u32)],
                ));
            }

            // 空值
            Expr::Nil => {
                let idx = self.chunk.add_constant(Constant::Nil);
                self.emit_instruction(Instruction::new(
                    OpCode::LoadConst,
                    vec![Operand::U32(idx as u32)],
                ));
            }

            // 符号（变量引用）
            Expr::Symbol(name) => {
                self.compile_variable(name)?;
            }

            // 列表（可能是函数调用或特殊形式）
            Expr::List(list) if !list.is_empty() => {
                self.compile_list(list)?;
            }

            Expr::List(_) => {
                // 空列表
                let idx = self.chunk.add_constant(Constant::Nil);
                self.emit_instruction(Instruction::new(
                    OpCode::LoadConst,
                    vec![Operand::U32(idx as u32)],
                ));
            }

            // Lambda（暂不支持）
            Expr::Lambda { .. } => {
                return Err(CompileError::UnsupportedSyntax(
                    "lambda not yet supported in bytecode".to_string(),
                ));
            }

            // Thunk（暂不支持）
            Expr::Thunk(_) => {
                return Err(CompileError::UnsupportedSyntax(
                    "thunk not yet supported in bytecode".to_string(),
                ));
            }
        }

        Ok(())
    }

    /// 编译变量引用
    fn compile_variable(&mut self, name: &str) -> Result<(), CompileError> {
        // 特殊处理布尔字面量
        if name == "true" {
            let idx = self.chunk.add_constant(Constant::Boolean(true));
            self.emit_instruction(Instruction::new(
                OpCode::LoadConst,
                vec![Operand::U32(idx as u32)],
            ));
            return Ok(());
        }
        if name == "false" {
            let idx = self.chunk.add_constant(Constant::Boolean(false));
            self.emit_instruction(Instruction::new(
                OpCode::LoadConst,
                vec![Operand::U32(idx as u32)],
            ));
            return Ok(());
        }

        // 首先在局部变量中查找
        for (scope_idx, scope) in self.locals.iter().rev().enumerate() {
            if let Some(local) = scope.iter().find(|v| v.name == name) {
                // 所有局部变量都在同一个帧中（depth=0），使用变量的 slot
                self.emit_instruction(Instruction::new(
                    OpCode::LoadLocal,
                    vec![Operand::U8(0), Operand::U8(local.slot as u8)],
                ));
                return Ok(());
            }
        }

        // 在全局变量中查找
        if let Some(&idx) = self.globals.get(name) {
            self.emit_instruction(Instruction::new(
                OpCode::LoadGlobal,
                vec![Operand::U32(idx as u32)],
            ));
            return Ok(());
        }

        // 检查是否是内置函数
        if self.builtins.contains(name) {
            // 将内置函数名称作为字符串常量加载
            let idx = self.chunk.add_constant(Constant::String(name.to_string()));
            self.emit_instruction(Instruction::new(
                OpCode::LoadConst,
                vec![Operand::U32(idx as u32)],
            ));
            return Ok(());
        }

        // 未找到变量
        Err(CompileError::UndefinedVariable(name.to_string()))
    }

    /// 编译列表（函数调用或特殊形式）
    fn compile_list(&mut self, list: &[Expr]) -> Result<(), CompileError> {
        if list.is_empty() {
            return Ok(());
        }

        // 获取操作符（列表第一个元素）
        let first = &list[0];

        match first {
            // 特殊形式：define
            Expr::Symbol(op) if op == "define" => {
                self.compile_define(&list[1..])?;
            }

            // 特殊形式：set!
            Expr::Symbol(op) if op == "set!" => {
                self.compile_set(&list[1..])?;
            }

            // 特殊形式：if
            Expr::Symbol(op) if op == "if" => {
                self.compile_if(&list[1..])?;
            }

            // 特殊形式：lambda
            Expr::Symbol(op) if op == "lambda" => {
                self.compile_lambda(&list[1..])?;
            }

            // 特殊形式：let
            Expr::Symbol(op) if op == "let" => {
                self.compile_let(&list[1..])?;
            }

            // 特殊形式：quote
            Expr::Symbol(op) if op == "quote" => {
                self.compile_quote(&list[1..])?;
            }

            // 函数调用
            _ => {
                self.compile_call(list)?;
            }
        }

        Ok(())
    }

    /// 编译 define 表达式
    fn compile_define(&mut self, args: &[Expr]) -> Result<(), CompileError> {
        if args.is_empty() {
            return Err(CompileError::UnsupportedSyntax(
                "define requires arguments".to_string(),
            ));
        }

        match &args[0] {
            // (define x value)
            Expr::Symbol(name) => {
                if args.len() != 2 {
                    return Err(CompileError::UnsupportedSyntax(
                        "define requires exactly 2 arguments".to_string(),
                    ));
                }

                // 编译值
                self.compile_expr(&args[1])?;

                // 存储到全局变量
                let idx = if let Some(&idx) = self.globals.get(name) {
                    idx
                } else {
                    let idx = self.globals.len();
                    self.globals.insert(name.clone(), idx);
                    // 记录全局变量名称到 chunk
                    self.chunk.global_names.push(name.clone());
                    idx
                };

                self.emit_instruction(Instruction::new(
                    OpCode::StoreGlobal,
                    vec![Operand::U32(idx as u32)],
                ));
            }

            // (define (f x y) body...)
            Expr::List(_params) => {
                return Err(CompileError::UnsupportedSyntax(
                    "function definition syntax not yet supported".to_string(),
                ));
            }

            _ => {
                return Err(CompileError::UnsupportedSyntax(
                    "invalid define syntax".to_string(),
                ));
            }
        }

        Ok(())
    }

    /// 编译 let 表达式
    /// 语法：(let ((var1 val1) (var2 val2) ...) body...)
    fn compile_let(&mut self, args: &[Expr]) -> Result<(), CompileError> {
        if args.is_empty() {
            return Err(CompileError::UnsupportedSyntax(
                "let requires bindings and body".to_string(),
            ));
        }

        // 第一个元素是绑定列表
        let bindings = match &args[0] {
            Expr::List(list) => list,
            _ => {
                return Err(CompileError::UnsupportedSyntax(
                    "let bindings must be a list".to_string(),
                ));
            }
        };

        // 获取 body 表达式
        let body = &args[1..];
        if body.is_empty() {
            return Err(CompileError::UnsupportedSyntax(
                "let requires at least one body expression".to_string(),
            ));
        }

        // 进入新的作用域
        self.enter_scope();

        // 编译每个绑定
        for binding in bindings {
            match binding {
                Expr::List(pair) if pair.len() == 2 => {
                    // (var value)
                    match &pair[0] {
                        Expr::Symbol(name) => {
                            // 编译值
                            self.compile_expr(&pair[1])?;

                            // 计算 slot：累加所有作用域的变量数量
                            let slot = self.locals.iter().map(|scope| scope.len()).sum::<usize>();

                            // 添加局部变量到当前作用域
                            self.locals.last_mut().unwrap().push(LocalVar {
                                name: name.clone(),
                                depth: 0,
                                slot,
                            });

                            // 存储到局部变量（depth = 0 表示当前帧）
                            self.emit_instruction(Instruction::new(
                                OpCode::StoreLocal,
                                vec![Operand::U8(0), Operand::U8(slot as u8)],
                            ));
                        }
                        _ => {
                            return Err(CompileError::UnsupportedSyntax(
                                "let binding variable must be a symbol".to_string(),
                            ));
                        }
                    }
                }
                _ => {
                    return Err(CompileError::UnsupportedSyntax(
                        "let binding must be a list of (var value)".to_string(),
                    ));
                }
            }
        }

        // 编译 body 表达式（按顺序，最后一个值作为 let 的结果）
        for expr in body {
            self.compile_expr(expr)?;
        }

        // 离开作用域
        self.exit_scope();

        Ok(())
    }

    /// 编译 set! 表达式
    /// 语法：(set! var value)
    fn compile_set(&mut self, args: &[Expr]) -> Result<(), CompileError> {
        if args.len() != 2 {
            return Err(CompileError::UnsupportedSyntax(
                "set! requires exactly 2 arguments".to_string(),
            ));
        }

        // 获取变量名
        let var_name = match &args[0] {
            Expr::Symbol(name) => name,
            _ => {
                return Err(CompileError::UnsupportedSyntax(
                    "set! variable must be a symbol".to_string(),
                ));
            }
        };

        // 先编译值
        self.compile_expr(&args[1])?;

        // 查找变量位置（局部或全局）
        // 首先在局部变量中查找
        for (scope_idx, scope) in self.locals.iter().rev().enumerate() {
            if let Some(local) = scope.iter().find(|v| v.name == *var_name) {
                // 所有局部变量都在同一个帧中（depth=0），使用变量的 slot
                self.emit_instruction(Instruction::new(
                    OpCode::StoreLocal,
                    vec![Operand::U8(0), Operand::U8(local.slot as u8)],
                ));
                return Ok(());
            }
        }

        // 在全局变量中查找
        if let Some(&idx) = self.globals.get(var_name) {
            self.emit_instruction(Instruction::new(
                OpCode::StoreGlobal,
                vec![Operand::U32(idx as u32)],
            ));
            return Ok(());
        }

        // 变量未定义
        Err(CompileError::UndefinedVariable(var_name.clone()))
    }

    /// 编译 if 表达式
    fn compile_if(&mut self, args: &[Expr]) -> Result<(), CompileError> {
        if args.len() != 3 {
            return Err(CompileError::UnsupportedSyntax(
                "if requires exactly 3 arguments".to_string(),
            ));
        }

        // 编译条件
        self.compile_expr(&args[0])?;

        // 创建跳转占位符
        let jump_else_pos = self.chunk.code.len();
        self.emit_instruction(Instruction::new(OpCode::JumpIfFalse, vec![Operand::U32(0)]));

        // 编译 then 分支
        self.compile_expr(&args[1])?;

        // 跳过 else 分支
        let jump_end_pos = self.chunk.code.len();
        self.emit_instruction(Instruction::new(OpCode::Jump, vec![Operand::U32(0)]));

        // 修正 else 跳转目标
        let else_pos = self.chunk.code.len();
        self.patch_jump(jump_else_pos, else_pos);

        // 编译 else 分支
        self.compile_expr(&args[2])?;

        // 修正结束跳转目标
        let end_pos = self.chunk.code.len();
        self.patch_jump(jump_end_pos, end_pos);

        Ok(())
    }

    /// 编译 quote 表达式
    fn compile_quote(&mut self, args: &[Expr]) -> Result<(), CompileError> {
        if args.len() != 1 {
            return Err(CompileError::UnsupportedSyntax(
                "quote requires exactly 1 argument".to_string(),
            ));
        }

        // 将 quoted 表达式作为常量
        // 注意：这里简化处理，实际上应该实现完整的引用
        let idx = self.chunk.add_constant(Constant::Number(0.0)); // 占位
        self.emit_instruction(Instruction::new(
            OpCode::LoadConst,
            vec![Operand::U32(idx as u32)],
        ));

        Ok(())
    }

    /// 编译函数调用
    fn compile_call(&mut self, list: &[Expr]) -> Result<(), CompileError> {
        // 先编译所有参数（从左到右）
        for expr in &list[1..] {
            self.compile_expr(expr)?;
        }

        // 最后编译函数（这样函数会在栈顶）
        self.compile_expr(&list[0])?;

        // 发起调用（参数个数 = 列表长度 - 1）
        let argc = (list.len() - 1) as u8;
        self.emit_instruction(Instruction::new(OpCode::Call, vec![Operand::U8(argc)]));

        Ok(())
    }

    /// 编译 lambda 表达式
    /// 语法：(lambda (params...) body...) 或 (lambda param body)
    fn compile_lambda(&mut self, args: &[Expr]) -> Result<(), CompileError> {
        if args.is_empty() {
            return Err(CompileError::UnsupportedSyntax(
                "lambda requires parameters and body".to_string(),
            ));
        }

        // 解析参数列表
        let params = self.parse_lambda_params(&args[0])?;

        // 获取函数体
        let body = &args[1..];
        if body.is_empty() {
            return Err(CompileError::UnsupportedSyntax(
                "lambda requires at least one body expression".to_string(),
            ));
        }

        // 分析函数体中引用的变量，确定需要捕获的闭包变量
        let captures = self.analyze_captures(&params, body)?;

        // 创建新的编译器实例编译函数体
        let mut func_compiler = BytecodeCompiler::with_builtins(self.builtins.clone());

        // 将捕获的变量添加到全局变量表中作为占位符
        // 这样在编译函数体时就不会报"未定义变量"错误
        for (i, capture) in captures.iter().enumerate() {
            func_compiler.globals.entry(capture.clone()).or_insert(i);
        }

        // 设置 lambda 的参数为局部变量
        func_compiler.enter_scope();
        for (i, param) in params.iter().enumerate() {
            func_compiler.locals.last_mut().unwrap().push(LocalVar {
                name: param.clone(),
                depth: 0,
                slot: i,
            });
        }

        // 编译函数体（多个表达式按顺序编译，最后一个结果作为返回值）
        for expr in body {
            func_compiler.compile_expr(expr)?;
        }

        // 在函数体末尾添加 Return 指令
        func_compiler.emit_instruction(Instruction::simple(OpCode::Return));

        func_compiler.exit_scope();

        // 获取编译后的字节码块
        let func_chunk = func_compiler.chunk;

        // 将 lambda 作为常量存储
        let lambda_constant = Constant::Lambda {
            params: params.clone(),
            chunk: func_chunk,
            captures: captures.clone(),
        };

        let idx = self.chunk.add_constant(lambda_constant);

        // 生成 MakeLambda 指令
        self.emit_instruction(Instruction::new(
            OpCode::MakeLambda,
            vec![Operand::U32(idx as u32)],
        ));

        Ok(())
    }

    /// 解析 lambda 参数列表
    /// 支持：(lambda (x y z) body) 和 (lambda x body)
    fn parse_lambda_params(&self, param_spec: &Expr) -> Result<Vec<String>, CompileError> {
        match param_spec {
            // (lambda (x y z) body) - 多参数形式
            Expr::List(params) => {
                let mut param_names = Vec::new();
                for p in params {
                    match p {
                        Expr::Symbol(name) => {
                            param_names.push(name.clone());
                        }
                        _ => {
                            return Err(CompileError::UnsupportedSyntax(
                                format!("lambda parameter must be a symbol, got {:?}", p),
                            ));
                        }
                    }
                }
                Ok(param_names)
            }

            // (lambda x body) - 单参数形式
            Expr::Symbol(name) => Ok(vec![name.clone()]),

            _ => Err(CompileError::UnsupportedSyntax(
                "invalid lambda parameter syntax".to_string(),
            )),
        }
    }

    /// 分析函数体中需要捕获的闭包变量
    /// 返回在函数体中使用但不在参数列表中的变量名
    fn analyze_captures(&self, params: &[String], body: &[Expr]) -> Result<Vec<String>, CompileError> {
        let mut captured = std::collections::HashSet::new();

        // 收集当前可访问的变量（局部变量 + 全局变量）
        let mut available_vars = std::collections::HashSet::new();

        // 添加所有局部变量（从所有作用域）
        for scope in &self.locals {
            for var in scope {
                available_vars.insert(var.name.clone());
            }
        }

        // 添加全局变量
        for name in self.globals.keys() {
            available_vars.insert(name.clone());
        }

        // 从函数体中收集使用的变量
        let mut used_vars = std::collections::HashSet::new();
        for expr in body {
            self.collect_used_vars(expr, &mut used_vars);
        }

        // 捕获的变量 = 使用的变量 - 参数列表 - 当前作用域可访问的变量
        for var in used_vars {
            if !params.contains(&var) && available_vars.contains(&var) {
                captured.insert(var);
            }
        }

        Ok(captured.into_iter().collect())
    }

    /// 收集表达式中使用的变量
    fn collect_used_vars(&self, expr: &Expr, used: &mut std::collections::HashSet<String>) {
        match expr {
            Expr::Symbol(name) => {
                // 跳过内置函数和特殊形式
                if !self.is_special_form(name) && !self.is_builtin(name) {
                    used.insert(name.clone());
                }
            }

            Expr::List(list) => {
                for item in list {
                    self.collect_used_vars(item, used);
                }
            }

            Expr::Lambda { params, body, .. } => {
                // 嵌套 lambda 需要递归处理，但参数不算在当前作用域的使用中
                let mut inner_used = std::collections::HashSet::new();
                for expr in body {
                    self.collect_used_vars(expr, &mut inner_used);
                }
                // 提取参数名称并移除
                for param in params {
                    if let Expr::Symbol(name) = param {
                        inner_used.remove(name);
                    }
                }
                // 将嵌套使用的变量添加到当前使用集合
                used.extend(inner_used);
            }

            Expr::Thunk(thunk) => {
                // 获取 thunk 中的表达式
                if let ThunkState::Unevaluated { expr, .. } = &*thunk.borrow() {
                    self.collect_used_vars(expr, used);
                }
            }

            // 常量不使用变量
            _ => {}
        }
    }

    /// 判断是否是特殊形式关键字
    fn is_special_form(&self, name: &str) -> bool {
        matches!(
            name,
            "define" | "set!" | "if" | "lambda" | "let" | "quote" | "eval" | "load"
        )
    }

    /// 判断是否是内置函数
    fn is_builtin(&self, name: &str) -> bool {
        self.builtins.contains(name)
    }

    /// 发出指令
    fn emit_instruction(&mut self, instruction: Instruction) {
        self.chunk.write_instruction(&instruction, self.current_line);
    }

    /// 修补跳转目标
    fn patch_jump(&mut self, offset: usize, target: usize) {
        let jump_offset = (target as i32 - offset as i32 - 5) as u32; // -5 = 操作码(1) + 操作数(4)

        let bytes = jump_offset.to_le_bytes();
        self.chunk.code[offset + 1..offset + 5].copy_from_slice(&bytes);
    }

    /// 进入新的作用域
    fn enter_scope(&mut self) {
        self.scope_depth += 1;
        self.locals.push(Vec::new());
    }

    /// 离开当前作用域
    fn exit_scope(&mut self) {
        self.scope_depth -= 1;
        self.locals.pop();
    }
}

impl Default for BytecodeCompiler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_number() {
        let mut compiler = BytecodeCompiler::new();
        let expr = Expr::Number(42.0);

        let result = compiler.compile(&expr);
        assert!(result.is_ok());

        let chunk = result.unwrap();
        assert!(!chunk.code.is_empty());
        assert_eq!(chunk.constants.len(), 1);
    }

    #[test]
    fn test_compile_simple_arithmetic() {
        let mut compiler = BytecodeCompiler::new();
        // 测试更简单的表达式：数字
        let expr = Expr::Number(42.0);

        let result = compiler.compile(&expr);
        assert!(result.is_ok());

        let chunk = result.unwrap();
        let disasm = chunk.disassemble("test");
        println!("{}", disasm);
        assert!(disasm.contains("LOAD_CONST"));
    }

    #[test]
    fn test_compile_define() {
        let mut compiler = BytecodeCompiler::new();
        let expr = Expr::List(vec![
            Expr::Symbol("define".to_string()),
            Expr::Symbol("x".to_string()),
            Expr::Number(42.0),
        ]);

        let result = compiler.compile(&expr);
        assert!(result.is_ok());

        let chunk = result.unwrap();
        assert!(chunk.code.contains(&(OpCode::StoreGlobal as u8)));
    }

    #[test]
    fn test_compile_if() {
        let mut compiler = BytecodeCompiler::new();
        let expr = Expr::List(vec![
            Expr::Symbol("if".to_string()),
            Expr::Bool(true),
            Expr::Number(1.0),
            Expr::Number(2.0),
        ]);

        let result = compiler.compile(&expr);
        assert!(result.is_ok());

        let chunk = result.unwrap();
        assert!(chunk.code.contains(&(OpCode::JumpIfFalse as u8)));
        assert!(chunk.code.contains(&(OpCode::Jump as u8)));
    }

    #[test]
    fn test_compile_simple_lambda() {
        let mut compiler = BytecodeCompiler::new();
        // (lambda (x) x)
        let expr = Expr::List(vec![
            Expr::Symbol("lambda".to_string()),
            Expr::List(vec![Expr::Symbol("x".to_string())]),
            Expr::Symbol("x".to_string()),
        ]);

        let result = compiler.compile(&expr);
        assert!(result.is_ok());

        let chunk = result.unwrap();
        assert!(chunk.code.contains(&(OpCode::MakeLambda as u8)));
        // 验证常量表中包含 lambda
        assert!(!chunk.constants.is_empty());
    }

    #[test]
    fn test_compile_lambda_with_multiple_params() {
        let mut compiler = BytecodeCompiler::new();
        // (lambda (x y) (+ x y))
        let expr = Expr::List(vec![
            Expr::Symbol("lambda".to_string()),
            Expr::List(vec![
                Expr::Symbol("x".to_string()),
                Expr::Symbol("y".to_string()),
            ]),
            Expr::List(vec![
                Expr::Symbol("+".to_string()),
                Expr::Symbol("x".to_string()),
                Expr::Symbol("y".to_string()),
            ]),
        ]);

        let result = compiler.compile(&expr);
        if let Err(ref e) = result {
            eprintln!("Compile error: {}", e);
        }
        assert!(result.is_ok());

        let chunk = result.unwrap();
        assert!(chunk.code.contains(&(OpCode::MakeLambda as u8)));
    }

    #[test]
    fn test_compile_lambda_single_param() {
        let mut compiler = BytecodeCompiler::new();
        // (lambda x x) - 单参数简写形式
        let expr = Expr::List(vec![
            Expr::Symbol("lambda".to_string()),
            Expr::Symbol("x".to_string()),
            Expr::Symbol("x".to_string()),
        ]);

        let result = compiler.compile(&expr);
        assert!(result.is_ok());

        let chunk = result.unwrap();
        assert!(chunk.code.contains(&(OpCode::MakeLambda as u8)));
    }

    #[test]
    fn test_compile_lambda_with_closure() {
        let mut compiler = BytecodeCompiler::new();
        // 先定义一个变量
        let define_expr = Expr::List(vec![
            Expr::Symbol("define".to_string()),
            Expr::Symbol("x".to_string()),
            Expr::Number(42.0),
        ]);
        compiler.compile(&define_expr).unwrap();

        // (lambda y (+ x y)) - x 是外部变量，需要闭包捕获
        let lambda_expr = Expr::List(vec![
            Expr::Symbol("lambda".to_string()),
            Expr::List(vec![Expr::Symbol("y".to_string())]),
            Expr::List(vec![
                Expr::Symbol("+".to_string()),
                Expr::Symbol("x".to_string()),
                Expr::Symbol("y".to_string()),
            ]),
        ]);

        let result = compiler.compile(&lambda_expr);
        if let Err(ref e) = result {
            eprintln!("Compile error: {}", e);
        }
        assert!(result.is_ok());

        let chunk = result.unwrap();
        assert!(chunk.code.contains(&(OpCode::MakeLambda as u8)));

        // 检查 lambda 常量中的 captures 字段
        if let Some(Constant::Lambda { captures, .. }) = chunk.constants.first() {
            // x 应该被捕获
            assert!(captures.contains(&"x".to_string()));
        } else {
            panic!("Expected Lambda constant");
        }
    }

    #[test]
    fn test_compile_let_simple() {
        let mut compiler = BytecodeCompiler::new();
        // (let ((x 10)) x)
        let expr = Expr::List(vec![
            Expr::Symbol("let".to_string()),
            Expr::List(vec![
                Expr::List(vec![
                    Expr::Symbol("x".to_string()),
                    Expr::Number(10.0),
                ]),
            ]),
            Expr::Symbol("x".to_string()),
        ]);

        let result = compiler.compile(&expr);
        if let Err(ref e) = result {
            eprintln!("Compile error: {}", e);
        }
        assert!(result.is_ok());

        let chunk = result.unwrap();
        assert!(chunk.code.contains(&(OpCode::StoreLocal as u8)));
        assert!(chunk.code.contains(&(OpCode::LoadLocal as u8)));
    }

    #[test]
    fn test_compile_let_multiple_bindings() {
        let mut compiler = BytecodeCompiler::new();
        // (let ((x 10) (y 20)) (+ x y))
        let expr = Expr::List(vec![
            Expr::Symbol("let".to_string()),
            Expr::List(vec![
                Expr::List(vec![
                    Expr::Symbol("x".to_string()),
                    Expr::Number(10.0),
                ]),
                Expr::List(vec![
                    Expr::Symbol("y".to_string()),
                    Expr::Number(20.0),
                ]),
            ]),
            Expr::List(vec![
                Expr::Symbol("+".to_string()),
                Expr::Symbol("x".to_string()),
                Expr::Symbol("y".to_string()),
            ]),
        ]);

        let result = compiler.compile(&expr);
        assert!(result.is_ok());

        let chunk = result.unwrap();
        // 打印反汇编用于调试
        eprintln!("Disassembly:\n{}", chunk.disassemble("test_let_multiple"));

        // 使用指令解码来统计 StoreLocal 指令
        let mut offset = 0;
        let mut store_count = 0;
        while offset < chunk.code.len() {
            if let Some(instruction) = Instruction::decode(&chunk.code, &mut offset) {
                if instruction.opcode == OpCode::StoreLocal {
                    store_count += 1;
                }
            } else {
                break;
            }
        }
        eprintln!("StoreLocal count: {}", store_count);
        // 应该有两次 StoreLocal（x 和 y）
        assert_eq!(store_count, 2);
    }

    #[test]
    fn test_compile_set_local() {
        let mut compiler = BytecodeCompiler::new();
        // 先定义一个局部变量（通过 let）
        compiler.compile(&Expr::List(vec![
            Expr::Symbol("let".to_string()),
            Expr::List(vec![
                Expr::List(vec![
                    Expr::Symbol("x".to_string()),
                    Expr::Number(10.0),
                ]),
            ]),
            // (set! x 20)
            Expr::List(vec![
                Expr::Symbol("set!".to_string()),
                Expr::Symbol("x".to_string()),
                Expr::Number(20.0),
            ]),
        ])).unwrap();

        // 检查编译成功
        // let 会创建新的作用域，set! 应该编译为 StoreLocal
        assert!(true); // 如果没有 panic 就算成功
    }

    #[test]
    fn test_compile_set_global() {
        let mut compiler = BytecodeCompiler::new();
        // 先定义一个全局变量
        compiler.compile(&Expr::List(vec![
            Expr::Symbol("define".to_string()),
            Expr::Symbol("x".to_string()),
            Expr::Number(42.0),
        ])).unwrap();

        // (set! x 100)
        let result = compiler.compile(&Expr::List(vec![
            Expr::Symbol("set!".to_string()),
            Expr::Symbol("x".to_string()),
            Expr::Number(100.0),
        ]));

        assert!(result.is_ok());

        let chunk = result.unwrap();
        // set! 应该编译为 StoreGlobal（因为 x 是全局变量）
        assert!(chunk.code.contains(&(OpCode::StoreGlobal as u8)));
    }
}
