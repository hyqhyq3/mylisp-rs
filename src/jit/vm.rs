//! 字节码虚拟机
//!
//! 执行 MyLisp 字节码的虚拟机。

use crate::ast::Expr;
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

    /// 局部变量（可选，用于闭包）
    locals: Vec<Expr>,
}

/// 字节码虚拟机
pub struct BytecodeVM {
    /// 字节码块
    chunk: Chunk,

    /// 值栈
    stack: Vec<Expr>,

    /// 调用帧栈
    frames: Vec<CallFrame>,

    /// 全局变量
    globals: HashMap<String, Expr>,

    /// 当前指令指针
    ip: usize,

    /// 最大栈大小
    max_stack_size: usize,
}

impl BytecodeVM {
    /// 创建新的虚拟机
    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            stack: Vec::new(),
            frames: Vec::new(),
            globals: HashMap::new(),
            ip: 0,
            max_stack_size: 1024, // 默认最大栈大小
        }
    }

    /// 设置最大栈大小
    pub fn set_max_stack_size(&mut self, size: usize) {
        self.max_stack_size = size;
    }

    /// 运行字节码直到完成
    pub fn run(&mut self) -> Result<Expr, VMError> {
        self.ip = 0;

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

        // 返回栈顶值作为结果
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
                let _depth = self.get_operand_u8(&instruction.operands, 0);
                let slot = self.get_operand_u8(&instruction.operands, 1) as usize;

                // 简化版本：直接从当前帧获取
                // TODO: 实现作用域链查找
                if let Some(frame) = self.frames.last() {
                    if slot < frame.locals.len() {
                        self.push(frame.locals[slot].clone());
                    } else {
                        return Err(VMError::Other(format!("Invalid local slot: {}", slot)));
                    }
                } else {
                    return Err(VMError::Other("No active frame".to_string()));
                }
            }

            OpCode::StoreLocal => {
                let _depth = self.get_operand_u8(&instruction.operands, 0);
                let slot = self.get_operand_u8(&instruction.operands, 1) as usize;
                let value = self.pop()?;

                if let Some(frame) = self.frames.last_mut() {
                    if slot < frame.locals.len() {
                        frame.locals[slot] = value;
                    } else {
                        return Err(VMError::Other(format!("Invalid local slot: {}", slot)));
                    }
                }
            }

            OpCode::LoadGlobal => {
                // 简化版本：暂不实现全局变量查找
                return Err(VMError::Other("LoadGlobal not yet implemented".to_string()));
            }

            OpCode::StoreGlobal => {
                let idx = self.get_operand_u32(&instruction.operands, 0) as usize;
                let value = self.pop()?;

                // 简化版本：暂不实现全局变量存储
                // 实际实现需要维护全局变量表
                let _ = (idx, value);
                return Err(VMError::Other("StoreGlobal not yet implemented".to_string()));
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
                let _argc = self.get_operand_u8(&instruction.operands, 0);
                return Err(VMError::Other("Call not yet implemented".to_string()));
            }

            OpCode::Return => {
                // 简化版本：直接返回栈顶值
                // 实际实现需要恢复调用帧
                return Ok(());
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
            Constant::Lambda { params, chunk, captures } => {
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
}
