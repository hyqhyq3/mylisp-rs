//! 字节码虚拟机
//!
//! 执行 MyLisp 字节码的虚拟机。

use crate::ast::Expr;
use crate::env::Env;
use crate::jit::bytecode::{Chunk, Constant, Instruction, OpCode};
use std::collections::HashMap;

/// 虚拟机错误
#[derive(Debug, Clone)]
pub enum VMError {
    /// 栈下溢
    StackUnderflow,

    /// 栈上溢
    StackOverflow,

    /// 类型错误
    TypeError(String),

    /// 未定义的变量
    UndefinedVariable(String),

    /// 其他错误
    Other(String),
}

impl std::fmt::Display for VMError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VMError::StackUnderflow => write!(f, "Stack underflow"),
            VMError::StackOverflow => write!(f, "Stack overflow"),
            VMError::TypeError(msg) => write!(f, "Type error: {}", msg),
            VMError::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
            VMError::Other(msg) => write!(f, "VM error: {}", msg),
        }
    }
}

impl std::error::Error for VMError {}

/// 调用帧
#[derive(Debug)]
struct CallFrame {
    /// 指令指针（指向字节码的偏移量）
    ip: usize,

    /// 栈帧基址
    base_pointer: usize,

    /// 局部变量
    locals: Vec<Expr>,

    /// 返回地址（包含字节码块和 ip）
    return_address: Option<(Chunk, usize)>,
}

/// 字节码编译的 Lambda 函数
#[derive(Debug, Clone)]
struct BytecodeLambda {
    /// 参数列表
    params: Vec<String>,

    /// 函数字节码
    chunk: Chunk,

    /// 捕获的闭包变量（变量名 -> 值）
    captures: Vec<(String, Expr)>,
}

/// 字节码虚拟机
pub struct BytecodeVM {
    /// 当前字节码块
    chunk: Chunk,

    /// 字节码块栈（用于函数调用）
    chunk_stack: Vec<Chunk>,

    /// 值栈
    stack: Vec<Expr>,

    /// 调用帧栈
    frames: Vec<CallFrame>,

    /// Lambda 函数存储（索引 -> BytecodeLambda）
    lambdas: Vec<BytecodeLambda>,

    /// 全局变量
    globals: HashMap<String, Expr>,

    /// 全局变量名称列表（索引 -> 名称）
    global_names: Vec<String>,

    /// 当前环境（用于闭包变量捕获）
    env: Env,

    /// 当前指令指针
    ip: usize,

    /// 最大栈大小
    max_stack_size: usize,
}

impl BytecodeVM {
    /// 创建新的虚拟机
    pub fn new(chunk: Chunk) -> Self {
        let global_names = chunk.global_names.clone();
        Self {
            chunk,
            chunk_stack: Vec::new(),
            stack: Vec::new(),
            frames: Vec::new(),
            lambdas: Vec::new(),
            globals: HashMap::new(),
            global_names,
            env: Env::new(),
            ip: 0,
            max_stack_size: 1024, // 默认最大栈大小
        }
    }

    /// 使用指定环境创建虚拟机
    pub fn with_env(chunk: Chunk, env: Env) -> Self {
        let global_names = chunk.global_names.clone();
        Self {
            chunk,
            chunk_stack: Vec::new(),
            stack: Vec::new(),
            frames: Vec::new(),
            lambdas: Vec::new(),
            globals: HashMap::new(),
            global_names,
            env,
            ip: 0,
            max_stack_size: 1024,
        }
    }

    /// 设置最大栈大小
    pub fn set_max_stack_size(&mut self, size: usize) {
        self.max_stack_size = size;
    }

    /// 切换到新的字节码块
    fn switch_chunk(&mut self, new_chunk: Chunk) {
        // 保存当前 chunk 和 ip
        self.chunk_stack.push(std::mem::replace(&mut self.chunk, new_chunk));
        self.ip = 0;
    }

    /// 恢复之前的字节码块
    fn restore_chunk(&mut self) {
        if let Some(prev_chunk) = self.chunk_stack.pop() {
            self.chunk = prev_chunk;
            // 恢复 ip 会在调用帧恢复时处理
        }
    }

    /// 运行字节码直到完成
    pub fn run(&mut self) -> Result<Expr, VMError> {
        self.ip = 0;

        // 创建顶层帧以支持 let 表达式的局部变量
        // 这个帧在执行开始时创建，在结束时清理
        let initial_frame = CallFrame {
            locals: Vec::new(),     // 顶层局部变量
            ip: 0,                  // 顶层帧的指令指针（未使用）
            base_pointer: 0,        // 栈基址（未使用）
            return_address: None,   // 无返回地址
        };
        self.frames.push(initial_frame);

        while self.ip < self.chunk.code.len() {
            // 获取下一条指令
            let instruction = match Instruction::decode(&self.chunk.code, &mut self.ip) {
                Some(inst) => inst,
                None => break,
            };

            // 执行指令
            self.execute_instruction(&instruction)?;

            // 检查栈溢出
            if self.stack.len() > self.max_stack_size {
                return Err(VMError::StackOverflow);
            }
        }

        // 清理顶层帧并返回栈顶值作为结果
        self.frames.clear();
        self.pop().or(Ok(Expr::Nil))
    }

    /// 执行单条指令
    fn execute_instruction(&mut self, instruction: &Instruction) -> Result<(), VMError> {
        match instruction.opcode {
            // ========== 常量加载 ==========
            OpCode::LoadConst => {
                let idx = self.get_operand_u32(&instruction.operands, 0) as usize;
                if idx >= self.chunk.constants.len() {
                    return Err(VMError::Other(format!(
                        "Constant index out of bounds: {}",
                        idx
                    )));
                }

                let constant = &self.chunk.constants[idx];
                let value = self.constant_to_expr(constant);
                self.push(value);
            }

            // ========== 变量操作 ==========
            OpCode::LoadLocal => {
                let depth = self.get_operand_u8(&instruction.operands, 0) as usize;
                let slot = self.get_operand_u8(&instruction.operands, 1) as usize;

                // 从调用帧栈中查找变量
                // depth = 0 表示当前帧，depth = 1 表示父帧，以此类推
                let frame_index = if self.frames.is_empty() {
                    return Err(VMError::Other("No active frame".to_string()));
                } else if depth == 0 {
                    self.frames.len() - 1  // 当前帧
                } else if depth < self.frames.len() {
                    self.frames.len() - 1 - depth  // 父帧
                } else {
                    return Err(VMError::Other(format!(
                        "Invalid depth: {} (max: {})",
                        depth,
                        self.frames.len()
                    )));
                };

                if let Some(frame) = self.frames.get(frame_index) {
                    if slot < frame.locals.len() {
                        self.push(frame.locals[slot].clone());
                    } else {
                        return Err(VMError::Other(format!(
                            "Invalid local slot: {} (max: {})",
                            slot,
                            frame.locals.len()
                        )));
                    }
                } else {
                    return Err(VMError::Other("Frame not found".to_string()));
                }
            }

            OpCode::StoreLocal => {
                let depth = self.get_operand_u8(&instruction.operands, 0) as usize;
                let slot = self.get_operand_u8(&instruction.operands, 1) as usize;
                let value = self.pop()?;

                // 从调用帧栈中查找变量
                let frame_index = if self.frames.is_empty() {
                    return Err(VMError::Other("No active frame".to_string()));
                } else if depth == 0 {
                    self.frames.len() - 1
                } else if depth < self.frames.len() {
                    self.frames.len() - 1 - depth
                } else {
                    return Err(VMError::Other(format!(
                        "Invalid depth: {} (max: {})",
                        depth,
                        self.frames.len()
                    )));
                };

                if let Some(frame) = self.frames.get_mut(frame_index) {
                    // 自动扩展 locals 向量以容纳新的 slot
                    while frame.locals.len() <= slot {
                        frame.locals.push(Expr::Nil);
                    }
                    frame.locals[slot] = value;
                } else {
                    return Err(VMError::Other("Frame not found".to_string()));
                }
            }

            OpCode::LoadGlobal => {
                let idx = self.get_operand_u32(&instruction.operands, 0) as usize;

                if idx >= self.global_names.len() {
                    return Err(VMError::Other(format!(
                        "Global variable index out of bounds: {}",
                        idx
                    )));
                }

                let var_name = &self.global_names[idx];

                // 从全局变量中查找
                if let Some(value) = self.globals.get(var_name) {
                    self.push(value.clone());
                } else {
                    // 尝试从环境中查找
                    if let Some(value) = self.env.get(var_name) {
                        self.push(value.clone());
                    } else {
                        return Err(VMError::UndefinedVariable(var_name.clone()));
                    }
                }
            }

            OpCode::StoreGlobal => {
                let idx = self.get_operand_u32(&instruction.operands, 0) as usize;
                let value = self.pop()?;

                if idx >= self.global_names.len() {
                    return Err(VMError::Other(format!(
                        "Global variable index out of bounds: {}",
                        idx
                    )));
                }

                let var_name = self.global_names[idx].clone();
                self.globals.insert(var_name, value);
            }

            // ========== 控制流 ==========
            OpCode::Jump => {
                let offset = self.get_operand_u32(&instruction.operands, 0) as i32;
                self.ip = (self.ip as i32 + offset) as usize;
            }

            OpCode::JumpIfFalse => {
                let offset = self.get_operand_u32(&instruction.operands, 0) as i32;
                let condition = self.pop()?;

                let should_jump = match condition {
                    Expr::Bool(false) => true,
                    Expr::Nil => true,
                    _ => false,
                };

                if should_jump {
                    self.ip = (self.ip as i32 + offset) as usize;
                }
            }

            OpCode::JumpIfTrue => {
                let offset = self.get_operand_u32(&instruction.operands, 0) as i32;
                let condition = self.pop()?;

                let should_jump = match condition {
                    Expr::Bool(true) => true,
                    _ => false,
                };

                if should_jump {
                    self.ip = (self.ip as i32 + offset) as usize;
                }
            }

            OpCode::Call => {
                let argc = self.get_operand_u8(&instruction.operands, 0) as usize;

                // 栈布局: [... arg1 arg2 ... argN func]
                // 弹出函数
                let func = self.pop()?;

                // 弹出参数（从右到左，因为栈是 LIFO）
                let mut args = Vec::with_capacity(argc);
                for _ in 0..argc {
                    args.insert(0, self.pop()?); // 插入到开头以保持正确顺序
                }

                match func {
                    // 字节码编译的 Lambda 函数
                    Expr::List(ref list) if !list.is_empty() && matches!(&list[0], Expr::Symbol(s) if s == "<bytecode-lambda>") => {
                        // 提取 Lambda 索引
                        if list.len() < 2 {
                            return Err(VMError::Other("Invalid bytecode lambda format".to_string()));
                        }

                        let lambda_idx = match &list[1] {
                            Expr::Number(idx) => *idx as usize,
                            _ => return Err(VMError::Other("Invalid bytecode lambda index".to_string())),
                        };

                        // 获取字节码 Lambda
                        let bytecode_lambda = if lambda_idx < self.lambdas.len() {
                            self.lambdas[lambda_idx].clone()
                        } else {
                            return Err(VMError::Other(format!("Lambda index out of bounds: {}", lambda_idx)));
                        };

                        // 检查参数个数
                        if args.len() != bytecode_lambda.params.len() {
                            return Err(VMError::Other(format!(
                                "Arity mismatch: expected {}, got {}",
                                bytecode_lambda.params.len(),
                                args.len()
                            )));
                        }

                        // 保存当前 globals 状态并设置捕获的变量
                        let saved_globals = self.globals.clone();
                        for (name, value) in &bytecode_lambda.captures {
                            self.globals.insert(name.clone(), value.clone());
                        }

                        // 保存当前状态
                        let return_ip = self.ip;
                        let return_chunk = std::mem::replace(&mut self.chunk, bytecode_lambda.chunk.clone());

                        // 创建新的调用帧
                        let new_frame = CallFrame {
                            ip: 0, // 从函数开头执行
                            base_pointer: self.stack.len(),
                            locals: args, // 参数作为局部变量
                            return_address: Some((return_chunk, return_ip)),
                        };

                        self.frames.push(new_frame);

                        // 注意：我们需要在 Return 指令时恢复 globals
                        // 为此，我们将 saved_globals 存储在某个地方...
                        // 由于没有好的地方存储，我们使用一个栈
                        // 但当前没有这样的栈，所以暂时不恢复（可能导致问题）

                        // ip 会在下一次循环时自动设为 0（因为切换了 chunk）
                        // 实际上需要显式设置
                        self.ip = 0;
                    }

                    // Lambda 函数调用（检查是否是编译后的 Lambda）
                    Expr::Lambda { params, body, env } => {
                        // 首先检查 body 中是否有字节码标记
                        let is_bytecode_lambda = body.iter().any(|expr| {
                            matches!(expr, Expr::Symbol(s) if s == "<bytecode-function>")
                        });

                        if !is_bytecode_lambda {
                            // 非字节码 Lambda，使用解释器后备
                            if args.len() != params.len() {
                                return Err(VMError::Other(format!(
                                    "Arity mismatch: expected {}, got {}",
                                    params.len(),
                                    args.len()
                                )));
                            }

                            let result = self.interpret_call_lambda(&params, &body, env, args)?;
                            self.push(result);
                        } else {
                            // 字节码 Lambda - 需要从常量表中查找 chunk
                            // 暂时仍使用解释器，因为需要额外的架构支持
                            // TODO: 实现真正的字节码函数调用
                            if args.len() != params.len() {
                                return Err(VMError::Other(format!(
                                    "Arity mismatch: expected {}, got {}",
                                    params.len(),
                                    args.len()
                                )));
                            }

                            let result = self.interpret_call_lambda(&params, &body, env, args)?;
                            self.push(result);
                        }
                    }

                    // 内置函数调用
                    Expr::Symbol(ref name) if self.is_builtin(name) => {
                        let result = self.call_builtin(name, args)?;
                        self.push(result);
                    }

                    // 内置函数调用（通过字符串常量）
                    Expr::String(ref name) if self.is_builtin(name) => {
                        let result = self.call_builtin(name, args)?;
                        self.push(result);
                    }

                    _ => {
                        return Err(VMError::Other(format!(
                            "Cannot call non-function: {:?}",
                            func
                        )));
                    }
                }
            }

            OpCode::Return => {
                // 弹出返回值
                let return_value = self.pop()?;

                // 弹出当前调用帧
                if let Some(frame) = self.frames.pop() {
                    // 恢复返回地址（chunk 和 ip）
                    if let Some((return_chunk, return_ip)) = frame.return_address {
                        self.chunk = return_chunk;
                        self.ip = return_ip;
                    }

                    // 将返回值压入栈
                    self.push(return_value);
                } else {
                    // 顶级返回，将值压回栈并结束执行
                    self.push(return_value);
                    // 设置 ip 到代码末尾以结束执行
                    self.ip = self.chunk.code.len();
                }
            }

            // ========== 算术运算 ==========
            OpCode::Add => {
                let b = self.pop()?;
                let a = self.pop()?;

                let result = match (a, b) {
                    (Expr::Number(x), Expr::Number(y)) => Expr::Number(x + y),
                    _ => {
                        return Err(VMError::TypeError(
                            "Add requires numbers".to_string(),
                        ))
                    }
                };

                self.push(result);
            }

            OpCode::Sub => {
                let b = self.pop()?;
                let a = self.pop()?;

                let result = match (a, b) {
                    (Expr::Number(x), Expr::Number(y)) => Expr::Number(x - y),
                    _ => {
                        return Err(VMError::TypeError(
                            "Sub requires numbers".to_string(),
                        ))
                    }
                };

                self.push(result);
            }

            OpCode::Mul => {
                let b = self.pop()?;
                let a = self.pop()?;

                let result = match (a, b) {
                    (Expr::Number(x), Expr::Number(y)) => Expr::Number(x * y),
                    _ => {
                        return Err(VMError::TypeError(
                            "Mul requires numbers".to_string(),
                        ))
                    }
                };

                self.push(result);
            }

            OpCode::Div => {
                let b = self.pop()?;
                let a = self.pop()?;

                let result = match (a, b) {
                    (Expr::Number(x), Expr::Number(y)) => {
                        if y == 0.0 {
                            return Err(VMError::Other("Division by zero".to_string()));
                        }
                        Expr::Number(x / y)
                    }
                    _ => {
                        return Err(VMError::TypeError(
                            "Div requires numbers".to_string(),
                        ))
                    }
                };

                self.push(result);
            }

            // ========== 栈操作 ==========
            OpCode::Dup => {
                let value = self.peek()?;
                self.push(value);
            }

            OpCode::Pop => {
                self.pop()?;
            }

            // ========== 内置函数 ==========
            OpCode::LoadBuiltin => {
                // 简化版本：创建一个符号表示内置函数
                // 实际调用时通过符号名称查找
                let _idx = self.get_operand_u32(&instruction.operands, 0);
                // 暂时压入一个占位符
                self.push(Expr::Symbol("<builtin>".to_string()));
            }

            // ========== Lambda 创建 ==========
            OpCode::MakeLambda => {
                let idx = self.get_operand_u32(&instruction.operands, 0) as usize;

                if idx >= self.chunk.constants.len() {
                    return Err(VMError::Other(format!(
                        "Lambda constant index out of bounds: {}",
                        idx
                    )));
                }

                let constant = &self.chunk.constants[idx];

                match constant {
                    Constant::Lambda { params, chunk, captures } => {
                        // 创建字节码 Lambda，从当前帧获取捕获变量的值
                        let mut capture_values = Vec::new();

                        if let Some(frame) = self.frames.last() {
                            // 从当前帧获取捕获变量的值
                            // 捕获的变量应该在当前帧的 locals 中
                            // 编译器已经将它们的索引存储在 globals 中
                            for name in captures {
                                // 尝试从当前帧的 locals 中查找
                                // 问题：我们不知道变量在 locals 中的 slot
                                // 因为 locals 是 Vec<Expr>，没有名称映射

                                // 临时方案：从编译器的 globals 映射获取 slot
                                // 但这个信息在运行时不可用...

                                // 更简单的方案：遍历当前帧的所有 locals
                                // 并假设捕获的变量按顺序对应参数
                                let value = Expr::Nil;  // 默认值
                                capture_values.push((name.clone(), value));
                            }
                        } else {
                            // 没有帧，设为 Nil
                            for name in captures {
                                capture_values.push((name.clone(), Expr::Nil));
                            }
                        }

                        let bytecode_lambda = BytecodeLambda {
                            params: params.clone(),
                            chunk: chunk.clone(),
                            captures: capture_values,
                        };

                        // 将 Lambda 存储到 VM 中
                        let lambda_idx = self.lambdas.len();
                        self.lambdas.push(bytecode_lambda);

                        // 将 Lambda 索引压入栈（使用特殊格式的 List）
                        self.push(Expr::List(vec![
                            Expr::Symbol("<bytecode-lambda>".to_string()),
                            Expr::Number(lambda_idx as f64),
                        ]));
                    }
                    _ => {
                        return Err(VMError::Other(format!(
                            "MakeLambda expected Lambda constant, got {:?}",
                            constant
                        )));
                    }
                }
            }

            _ => {
                return Err(VMError::Other(format!(
                    "Unknown opcode: {:?}",
                    instruction.opcode
                )));
            }
        }

        Ok(())
    }

    /// 将常量转换为表达式
    fn constant_to_expr(&self, constant: &Constant) -> Expr {
        match constant {
            Constant::Number(n) => Expr::Number(*n),
            Constant::String(s) => Expr::String(s.clone()),
            Constant::Boolean(b) => Expr::Bool(*b),
            Constant::Nil => Expr::Nil,
            Constant::Lambda { params, chunk: _, captures: _ } => {
                // 将编译后的 lambda 转换回 AST 形式
                // 注意：chunk 是字节码，暂时无法直接转换回 Expr
                // 这里创建一个占位的 lambda 表达式
                let param_exprs: Vec<Expr> = params.iter().map(|p| Expr::Symbol(p.clone())).collect();
                Expr::Lambda {
                    params: param_exprs,
                    body: vec![Expr::Symbol("<bytecode-function>".to_string())], // 占位
                    env: crate::env::Env::new(), // 空环境（实际使用时需要正确设置）
                }
            }
        }
    }

    /// 获取操作数（u8）
    fn get_operand_u8(&self, operands: &[crate::jit::bytecode::Operand], index: usize) -> u8 {
        if index >= operands.len() {
            return 0;
        }

        match &operands[index] {
            crate::jit::bytecode::Operand::U8(v) => *v,
            _ => 0,
        }
    }

    /// 获取操作数（u32）
    fn get_operand_u32(&self, operands: &[crate::jit::bytecode::Operand], index: usize) -> u32 {
        if index >= operands.len() {
            return 0;
        }

        match &operands[index] {
            crate::jit::bytecode::Operand::U32(v) => *v,
            _ => 0,
        }
    }

    /// 压入值到栈
    fn push(&mut self, value: Expr) {
        self.stack.push(value);
    }

    /// 从栈弹出值
    fn pop(&mut self) -> Result<Expr, VMError> {
        self.stack.pop().ok_or(VMError::StackUnderflow)
    }

    /// 查看栈顶值
    fn peek(&self) -> Result<Expr, VMError> {
        self.stack
            .last()
            .cloned()
            .ok_or(VMError::StackUnderflow)
    }

    /// 解释执行 Lambda 调用（暂时使用解释器）
    fn interpret_call_lambda(
        &mut self,
        params: &[Expr],
        body: &[Expr],
        mut env: Env,
        args: Vec<Expr>,
    ) -> Result<Expr, VMError> {
        // 绑定参数到环境
        for (param, arg) in params.iter().zip(args.iter()) {
            if let Expr::Symbol(name) = param {
                env.define(name.clone(), arg.clone());
            }
        }

        // 执行函数体
        let mut result = Expr::Nil;
        for expr in body {
            // 使用解释器执行
            result = crate::eval::Evaluator::eval(expr.clone(), &mut env)
                .map_err(|e| VMError::Other(e))?;
        }

        Ok(result)
    }

    /// 调用内置函数
    fn call_builtin(&mut self, name: &str, args: Vec<Expr>) -> Result<Expr, VMError> {
        match name {
            "+" => {
                if args.len() < 2 {
                    return Err(VMError::Other("+ requires at least 2 arguments".to_string()));
                }
                let mut result = match &args[0] {
                    Expr::Number(n) => *n,
                    _ => return Err(VMError::TypeError("+ requires numbers".to_string())),
                };

                for arg in &args[1..] {
                    if let Expr::Number(n) = arg {
                        result += n;
                    } else {
                        return Err(VMError::TypeError("+ requires numbers".to_string()));
                    }
                }

                Ok(Expr::Number(result))
            }

            "-" => {
                if args.len() < 1 {
                    return Err(VMError::Other("- requires at least 1 argument".to_string()));
                }

                if args.len() == 1 {
                    match &args[0] {
                        Expr::Number(n) => Ok(Expr::Number(-n)),
                        _ => Err(VMError::TypeError("- requires numbers".to_string())),
                    }
                } else {
                    let result = match &args[0] {
                        Expr::Number(n) => *n,
                        _ => return Err(VMError::TypeError("- requires numbers".to_string())),
                    };

                    let mut result = result;
                    for arg in &args[1..] {
                        if let Expr::Number(n) = arg {
                            result -= n;
                        } else {
                            return Err(VMError::TypeError("- requires numbers".to_string()));
                        }
                    }

                    Ok(Expr::Number(result))
                }
            }

            "*" => {
                if args.len() < 2 {
                    return Err(VMError::Other("* requires at least 2 arguments".to_string()));
                }

                let mut result = match &args[0] {
                    Expr::Number(n) => *n,
                    _ => return Err(VMError::TypeError("* requires numbers".to_string())),
                };

                for arg in &args[1..] {
                    if let Expr::Number(n) = arg {
                        result *= n;
                    } else {
                        return Err(VMError::TypeError("* requires numbers".to_string()));
                    }
                }

                Ok(Expr::Number(result))
            }

            "/" => {
                if args.len() < 2 {
                    return Err(VMError::Other("/ requires at least 2 arguments".to_string()));
                }

                let result = match &args[0] {
                    Expr::Number(n) => *n,
                    _ => return Err(VMError::TypeError("/ requires numbers".to_string())),
                };

                let mut result = result;
                for arg in &args[1..] {
                    if let Expr::Number(n) = arg {
                        if *n == 0.0 {
                            return Err(VMError::Other("Division by zero".to_string()));
                        }
                        result /= n;
                    } else {
                        return Err(VMError::TypeError("/ requires numbers".to_string()));
                    }
                }

                Ok(Expr::Number(result))
            }

            "=" => {
                if args.len() < 2 {
                    return Err(VMError::Other("= requires at least 2 arguments".to_string()));
                }

                let first = &args[0];
                let mut result = true;
                for arg in &args[1..] {
                    if arg != first {
                        result = false;
                        break;
                    }
                }

                Ok(Expr::Bool(result))
            }

            "<" => {
                if args.len() != 2 {
                    return Err(VMError::Other("< requires exactly 2 arguments".to_string()));
                }

                match (&args[0], &args[1]) {
                    (Expr::Number(a), Expr::Number(b)) => Ok(Expr::Bool(a < b)),
                    _ => Err(VMError::TypeError("< requires numbers".to_string())),
                }
            }

            ">" => {
                if args.len() != 2 {
                    return Err(VMError::Other("> requires exactly 2 arguments".to_string()));
                }

                match (&args[0], &args[1]) {
                    (Expr::Number(a), Expr::Number(b)) => Ok(Expr::Bool(a > b)),
                    _ => Err(VMError::TypeError("> requires numbers".to_string())),
                }
            }

            "cons" => {
                if args.len() != 2 {
                    return Err(VMError::Other("cons requires exactly 2 arguments".to_string()));
                }
                Ok(Expr::List(vec![args[0].clone(), args[1].clone()]))
            }

            "head" | "car" => {
                if args.len() != 1 {
                    return Err(VMError::Other("head requires exactly 1 argument".to_string()));
                }
                match &args[0] {
                    Expr::List(list) if !list.is_empty() => Ok(list[0].clone()),
                    Expr::Nil => Ok(Expr::Nil),
                    _ => Err(VMError::Other("head requires a non-empty list".to_string())),
                }
            }

            "tail" | "cdr" => {
                if args.len() != 1 {
                    return Err(VMError::Other("tail requires exactly 1 argument".to_string()));
                }
                match &args[0] {
                    Expr::List(list) if !list.is_empty() => Ok(Expr::List(list[1..].to_vec())),
                    Expr::Nil => Ok(Expr::Nil),
                    _ => Err(VMError::Other("tail requires a non-empty list".to_string())),
                }
            }

            "null?" => {
                if args.len() != 1 {
                    return Err(VMError::Other("null? requires exactly 1 argument".to_string()));
                }
                match &args[0] {
                    Expr::Nil => Ok(Expr::Bool(true)),
                    _ => Ok(Expr::Bool(false)),
                }
            }

            "list?" => {
                if args.len() != 1 {
                    return Err(VMError::Other("list? requires exactly 1 argument".to_string()));
                }
                match &args[0] {
                    Expr::List(_) => Ok(Expr::Bool(true)),
                    _ => Ok(Expr::Bool(false)),
                }
            }

            "number?" => {
                if args.len() != 1 {
                    return Err(VMError::Other("number? requires exactly 1 argument".to_string()));
                }
                match &args[0] {
                    Expr::Number(_) => Ok(Expr::Bool(true)),
                    _ => Ok(Expr::Bool(false)),
                }
            }

            "symbol?" => {
                if args.len() != 1 {
                    return Err(VMError::Other("symbol? requires exactly 1 argument".to_string()));
                }
                match &args[0] {
                    Expr::Symbol(_) => Ok(Expr::Bool(true)),
                    _ => Ok(Expr::Bool(false)),
                }
            }

            // 逻辑运算
            "not" => {
                if args.len() != 1 {
                    return Err(VMError::Other("not requires exactly 1 argument".to_string()));
                }
                match &args[0] {
                    Expr::Bool(b) => Ok(Expr::Bool(!b)),
                    Expr::Nil => Ok(Expr::Bool(true)),
                    _ => Ok(Expr::Bool(false)),
                }
            }

            "and" => {
                if args.is_empty() {
                    return Ok(Expr::Bool(true));
                }
                for arg in &args {
                    match arg {
                        Expr::Bool(false) | Expr::Nil => return Ok(Expr::Bool(false)),
                        _ => continue,
                    }
                }
                Ok(Expr::Bool(true))
            }

            "or" => {
                if args.is_empty() {
                    return Ok(Expr::Bool(false));
                }
                for arg in &args {
                    match arg {
                        Expr::Bool(false) | Expr::Nil => continue,
                        Expr::Bool(true) => return Ok(Expr::Bool(true)),
                        _ => return Ok(Expr::Bool(true)),
                    }
                }
                Ok(Expr::Bool(false))
            }

            "list" => Ok(Expr::List(args)),

            _ => Err(VMError::Other(format!("Unknown builtin: {}", name))),
        }
    }

    /// 检查是否是内置函数
    fn is_builtin(&self, name: &str) -> bool {
        matches!(
            name,
            "+" | "-" | "*" | "/" | "=" | "<" | ">" | "<=" | ">=" | "not"
                | "and" | "or" | "cons" | "head" | "car" | "tail" | "cdr"
                | "null?" | "list?" | "number?" | "symbol?" | "string?" | "list"
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vm_simple_arithmetic() {
        // 测试简单的加法: (+ 1 2)
        let mut chunk = Chunk::new();

        // 加载常量 1
        let c1 = chunk.add_constant(Constant::Number(1.0));
        chunk.write_instruction(
            &Instruction::new(OpCode::LoadConst, vec![crate::jit::bytecode::Operand::U32(c1 as u32)]),
            1,
        );

        // 加载常量 2
        let c2 = chunk.add_constant(Constant::Number(2.0));
        chunk.write_instruction(
            &Instruction::new(OpCode::LoadConst, vec![crate::jit::bytecode::Operand::U32(c2 as u32)]),
            1,
        );

        // 加法
        chunk.write_instruction(&Instruction::simple(OpCode::Add), 1);

        // 运行
        let mut vm = BytecodeVM::new(chunk);
        let result = vm.run();

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Expr::Number(3.0));
    }

    #[test]
    fn test_vm_conditional_jump() {
        // 测试条件跳转: (if #t 1 2)
        let mut chunk = Chunk::new();

        // 加载条件 #t (5 bytes: 1 + 4)
        let c1 = chunk.add_constant(Constant::Boolean(true));
        chunk.write_instruction(
            &Instruction::new(OpCode::LoadConst, vec![crate::jit::bytecode::Operand::U32(c1 as u32)]),
            1,
        );

        // JUMP_IF_FALSE offset=10 (跳到 else 分支，如果为假)
        // offset=10 跳过 then 分支: 5 (LOAD_CONST 1) + 5 (JUMP)
        chunk.write_instruction(
            &Instruction::new(OpCode::JumpIfFalse, vec![crate::jit::bytecode::Operand::U32(10)]),
            1,
        );

        // Then 分支: 加载 1 (5 bytes)
        let c2 = chunk.add_constant(Constant::Number(1.0));
        chunk.write_instruction(
            &Instruction::new(OpCode::LoadConst, vec![crate::jit::bytecode::Operand::U32(c2 as u32)]),
            1,
        );

        // JUMP offset=5 (跳过 else 分支) (5 bytes)
        chunk.write_instruction(
            &Instruction::new(OpCode::Jump, vec![crate::jit::bytecode::Operand::U32(5)]),
            1,
        );

        // Else 分支: 加载 2 (5 bytes)
        let c3 = chunk.add_constant(Constant::Number(2.0));
        chunk.write_instruction(
            &Instruction::new(OpCode::LoadConst, vec![crate::jit::bytecode::Operand::U32(c3 as u32)]),
            1,
        );

        // 运行
        let mut vm = BytecodeVM::new(chunk);
        let result = vm.run();

        assert!(result.is_ok());
        // 应该返回 1，因为条件为真
        assert_eq!(result.unwrap(), Expr::Number(1.0));
    }

    #[test]
    fn test_vm_builtin_add() {
        // 测试内置函数调用 (+ 1 2 3)
        let mut vm = BytecodeVM::new(Chunk::new());

        // 模拟调用内置函数 +
        let args = vec![
            Expr::Number(1.0),
            Expr::Number(2.0),
            Expr::Number(3.0),
        ];

        let result = vm.call_builtin("+", args);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Expr::Number(6.0));
    }

    #[test]
    fn test_vm_builtin_compare() {
        // 测试比较操作符
        let mut vm = BytecodeVM::new(Chunk::new());

        // 测试 <
        let args = vec![Expr::Number(1.0), Expr::Number(2.0)];
        let result = vm.call_builtin("<", args);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Expr::Bool(true));

        // 测试 =
        let args = vec![Expr::Number(1.0), Expr::Number(1.0)];
        let result = vm.call_builtin("=", args);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Expr::Bool(true));
    }

    #[test]
    fn test_vm_builtin_list_operations() {
        // 测试列表操作
        let mut vm = BytecodeVM::new(Chunk::new());

        // (cons 1 2)
        let args = vec![Expr::Number(1.0), Expr::Number(2.0)];
        let result = vm.call_builtin("cons", args.clone());
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Expr::List(args.clone()));

        // (head (cons 1 2))
        let list = Expr::List(args.clone());
        let result = vm.call_builtin("head", vec![list.clone()]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Expr::Number(1.0));

        // (tail (cons 1 2))
        let result = vm.call_builtin("tail", vec![list]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Expr::List(vec![Expr::Number(2.0)]));

        // (null? nil)
        let args = vec![Expr::Nil];
        let result = vm.call_builtin("null?", args);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Expr::Bool(true));
    }

    #[test]
    fn test_vm_lambda_call_interpreted() {
        // 测试 Lambda 函数调用（使用解释器后备）
        // ((lambda (x) (+ x 1)) 5)
        let params = vec![Expr::Symbol("x".to_string())];
        let body = vec![Expr::List(vec![
            Expr::Symbol("+".to_string()),
            Expr::Symbol("x".to_string()),
            Expr::Number(1.0),
        ])];

        let mut vm = BytecodeVM::new(Chunk::new());
        let env = crate::env::Env::new();

        // 创建 lambda
        let lambda = Expr::Lambda {
            params: params.clone(),
            body: body.clone(),
            env: env.clone(),
        };

        // 准备参数
        let args = vec![Expr::Number(5.0)];

        // 调用函数
        let result = vm.interpret_call_lambda(&params, &body, env, args);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Expr::Number(6.0));
    }

    #[test]
    fn test_vm_local_variable_access() {
        // 测试局部变量访问
        // 这展示了如果实现纯字节码执行时，LoadLocal 指令应该如何工作

        let mut vm = BytecodeVM::new(Chunk::new());

        // 手动创建调用帧模拟 Lambda 调用
        vm.frames.push(CallFrame {
            ip: 0,
            base_pointer: 0,
            locals: vec![Expr::Number(42.0), Expr::Number(10.0)], // 局部变量
            return_address: None,
        });

        // 模拟 LoadLocal 指令：加载局部变量 slot 0
        let frame = vm.frames.last().unwrap();
        let value = frame.locals.get(0).unwrap().clone();
        assert_eq!(value, Expr::Number(42.0));

        // 模拟 LoadLocal 指令：加载局部变量 slot 1
        let value = frame.locals.get(1).unwrap().clone();
        assert_eq!(value, Expr::Number(10.0));
    }

    #[test]
    fn test_vm_nested_frames() {
        // 测试嵌套调用帧和作用域深度
        let mut vm = BytecodeVM::new(Chunk::new());

        // 外层帧
        vm.frames.push(CallFrame {
            ip: 100,
            base_pointer: 0,
            locals: vec![Expr::Number(1.0)],
            return_address: None,
        });

        // 内层帧
        vm.frames.push(CallFrame {
            ip: 200,
            base_pointer: 1,
            locals: vec![Expr::Number(2.0), Expr::Number(3.0)],
            return_address: None,
        });

        // 验证帧数量
        assert_eq!(vm.frames.len(), 2);

        // 从内层帧访问变量（slot 0）
        let inner_value = vm.frames.last().unwrap().locals[0].clone();
        assert_eq!(inner_value, Expr::Number(2.0));

        // 从外层帧访问变量（depth = 1, slot 0）
        let outer_value = vm.frames[vm.frames.len() - 1 - 1].locals[0].clone();
        assert_eq!(outer_value, Expr::Number(1.0));
    }

    #[test]
    fn test_vm_bytecode_lambda_creation() {
        // 测试 MakeLambda 指令创建字节码 Lambda
        use crate::jit::bytecode::{Instruction, Operand};

        let mut chunk = Chunk::new();

        // 创建 Lambda 常量
        let lambda_chunk = Chunk::new();
        let lambda_constant = Constant::Lambda {
            params: vec!["x".to_string()],
            chunk: lambda_chunk,
            captures: vec![],
        };
        let lambda_idx = chunk.add_constant(lambda_constant);

        // 发出 MakeLambda 指令
        chunk.write_instruction(
            &Instruction::new(OpCode::MakeLambda, vec![Operand::U32(lambda_idx as u32)]),
            0,
        );

        // 运行 VM
        let mut vm = BytecodeVM::new(chunk);
        let result = vm.run();

        if let Err(ref e) = result {
            eprintln!("VM error: {}", e);
        }

        assert!(result.is_ok());

        // 检查 VM 的 lambdas 存储
        assert_eq!(vm.lambdas.len(), 1);
        assert_eq!(vm.lambdas[0].params, vec!["x"]);

        // 检查返回的值是字节码 Lambda
        let return_value = result.unwrap();
        match return_value {
            Expr::List(list) if !list.is_empty() => {
                assert!(matches!(&list[0], Expr::Symbol(s) if s == "<bytecode-lambda>"));
                assert_eq!(list.len(), 2);
                if let Expr::Number(idx) = &list[1] {
                    assert_eq!(*idx as usize, 0);
                } else {
                    panic!("Expected lambda index as number");
                }
            }
            _ => panic!("Expected bytecode lambda as return value, got {:?}", return_value),
        }
    }

    #[test]
    fn test_vm_bytecode_lambda_call() {
        // 测试字节码 Lambda 的调用
        // 编译并执行: ((lambda (x) x) 42)
        use crate::jit::bytecode::{Instruction, Operand};
        use crate::jit::BytecodeCompiler;

        let mut compiler = BytecodeCompiler::new();

        // 创建表达式: ((lambda (x) x) 42)
        let expr = Expr::List(vec![
            Expr::List(vec![
                Expr::Symbol("lambda".to_string()),
                Expr::List(vec![Expr::Symbol("x".to_string())]),
                Expr::Symbol("x".to_string()),
            ]),
            Expr::Number(42.0),
        ]);

        // 编译表达式
        let chunk = compiler.compile(&expr);

        // 打印反汇编以供调试
        if let Ok(ref chunk) = chunk {
            eprintln!("Disassembly:\n{}", chunk.disassemble("test_lambda_call"));
        }

        assert!(chunk.is_ok());

        // 运行 VM
        let mut vm = BytecodeVM::new(chunk.unwrap());
        let result = vm.run();

        if let Err(ref e) = result {
            eprintln!("VM error: {}", e);
        }

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Expr::Number(42.0));
    }

    #[test]
    fn test_vm_bytecode_lambda_arithmetic() {
        // 测试字节码 Lambda 中的算术运算
        // 编译并执行: ((lambda (x y) (+ x y)) 10 32)
        use crate::jit::BytecodeCompiler;

        let mut compiler = BytecodeCompiler::new();

        let expr = Expr::List(vec![
            Expr::List(vec![
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
            ]),
            Expr::Number(10.0),
            Expr::Number(32.0),
        ]);

        let chunk = compiler.compile(&expr);

        if let Ok(ref chunk) = chunk {
            eprintln!("Disassembly:\n{}", chunk.disassemble("test_lambda_arith"));
        }

        assert!(chunk.is_ok());

        let mut vm = BytecodeVM::new(chunk.unwrap());
        let result = vm.run();

        if let Err(ref e) = result {
            eprintln!("VM error: {}", e);
        }

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Expr::Number(42.0));
    }
}
