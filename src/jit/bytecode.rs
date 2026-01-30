//! 字节码定义和编码
//!
//! 定义 MyLisp 虚拟机的指令集和编码格式。

use std::fmt;

/// 字节码操作码
///
/// 每个操作码都是一个单字节值（0-255），用于标识要执行的操作。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum OpCode {
    // ========== 常量和变量加载 ==========
    /// 加载常量（操作数：常量索引）
    LoadConst = 0,

    /// 加载局部变量（操作数：深度、偏移量）
    LoadLocal,

    /// 存储局部变量（操作数：深度、偏移量）
    StoreLocal,

    /// 加载全局变量（操作数：变量索引）
    LoadGlobal,

    /// 存储全局变量（操作数：变量索引）
    StoreGlobal,

    // ========== 控制流 ==========
    /// 无条件跳转（操作数：跳转偏移量）
    Jump = 10,

    /// 条件跳转 - 如果为假则跳转（操作数：跳转偏移量）
    JumpIfFalse,

    /// 条件跳转 - 如果为真则跳转（操作数：跳转偏移量）
    JumpIfTrue,

    /// 函数调用（操作数：参数个数）
    Call,

    /// 返回
    Return,

    /// 循环/尾调用优化（操作数：参数个数）
    TailCall,

    // ========== 算术运算 ==========
    /// 加法
    Add = 20,

    /// 减法
    Sub,

    /// 乘法
    Mul,

    /// 除法
    Div,

    /// 取模
    Mod,

    /// 取反
    Neg,

    // ========== 比较运算 ==========
    /// 相等
    Eq = 30,

    /// 小于
    Lt,

    /// 大于
    Gt,

    /// 小于等于
    Le,

    /// 大于等于
    Ge,

    // ========== 逻辑运算 ==========
    /// 逻辑非
    Not = 40,

    /// 逻辑与
    And,

    /// 逻辑或
    Or,

    // ========== 列表操作 ==========
    /// 构造列表对 (cons)
    Cons = 50,

    /// 获取列表头部 (car/head)
    Head,

    /// 获取列表尾部 (cdr/tail)
    Tail,

    /// 判断是否为空列表
    NilP,

    /// 获取列表长度
    Length,

    /// 追加列表
    Append,

    // ========== Lambda 和闭包 ==========
    /// 创建 Lambda 函数（操作数：参数个数、body 开始位置）
    MakeLambda = 60,

    /// 闭包变量捕获
    CloseOver,

    // ========== 栈操作 ==========
    /// 复制栈顶元素
    Dup = 70,

    /// 弹出栈顶元素
    Pop,

    /// 交换栈顶两个元素
    Swap,

    // ========== 其他 ==========
    /// 加载内置函数（操作数：内置函数索引）
    LoadBuiltin = 80,

    /// 构建列表（操作数：元素个数）
    BuildList,
}

impl OpCode {
    /// 从字节转换为操作码
    pub fn from_byte(byte: u8) -> Option<Self> {
        match byte {
            0 => Some(OpCode::LoadConst),
            1 => Some(OpCode::LoadLocal),
            2 => Some(OpCode::StoreLocal),
            3 => Some(OpCode::LoadGlobal),
            4 => Some(OpCode::StoreGlobal),

            10 => Some(OpCode::Jump),
            11 => Some(OpCode::JumpIfFalse),
            12 => Some(OpCode::JumpIfTrue),
            13 => Some(OpCode::Call),
            14 => Some(OpCode::Return),
            15 => Some(OpCode::TailCall),

            20 => Some(OpCode::Add),
            21 => Some(OpCode::Sub),
            22 => Some(OpCode::Mul),
            23 => Some(OpCode::Div),
            24 => Some(OpCode::Mod),
            25 => Some(OpCode::Neg),

            30 => Some(OpCode::Eq),
            31 => Some(OpCode::Lt),
            32 => Some(OpCode::Gt),
            33 => Some(OpCode::Le),
            34 => Some(OpCode::Ge),

            40 => Some(OpCode::Not),
            41 => Some(OpCode::And),
            42 => Some(OpCode::Or),

            50 => Some(OpCode::Cons),
            51 => Some(OpCode::Head),
            52 => Some(OpCode::Tail),
            53 => Some(OpCode::NilP),
            54 => Some(OpCode::Length),
            55 => Some(OpCode::Append),

            60 => Some(OpCode::MakeLambda),
            61 => Some(OpCode::CloseOver),

            70 => Some(OpCode::Dup),
            71 => Some(OpCode::Pop),
            72 => Some(OpCode::Swap),

            80 => Some(OpCode::LoadBuiltin),
            81 => Some(OpCode::BuildList),

            _ => None,
        }
    }

    /// 获取操作码的助记符
    pub fn mnemonic(self) -> &'static str {
        match self {
            OpCode::LoadConst => "LOAD_CONST",
            OpCode::LoadLocal => "LOAD_LOCAL",
            OpCode::StoreLocal => "STORE_LOCAL",
            OpCode::LoadGlobal => "LOAD_GLOBAL",
            OpCode::StoreGlobal => "STORE_GLOBAL",

            OpCode::Jump => "JUMP",
            OpCode::JumpIfFalse => "JUMP_IF_FALSE",
            OpCode::JumpIfTrue => "JUMP_IF_TRUE",
            OpCode::Call => "CALL",
            OpCode::Return => "RET",
            OpCode::TailCall => "TAIL_CALL",

            OpCode::Add => "ADD",
            OpCode::Sub => "SUB",
            OpCode::Mul => "MUL",
            OpCode::Div => "DIV",
            OpCode::Mod => "MOD",
            OpCode::Neg => "NEG",

            OpCode::Eq => "EQ",
            OpCode::Lt => "LT",
            OpCode::Gt => "GT",
            OpCode::Le => "LE",
            OpCode::Ge => "GE",

            OpCode::Not => "NOT",
            OpCode::And => "AND",
            OpCode::Or => "OR",

            OpCode::Cons => "CONS",
            OpCode::Head => "HEAD",
            OpCode::Tail => "TAIL",
            OpCode::NilP => "NIL_P",
            OpCode::Length => "LENGTH",
            OpCode::Append => "APPEND",

            OpCode::MakeLambda => "MAKE_LAMBDA",
            OpCode::CloseOver => "CLOSE_OVER",

            OpCode::Dup => "DUP",
            OpCode::Pop => "POP",
            OpCode::Swap => "SWAP",

            OpCode::LoadBuiltin => "LOAD_BUILTIN",
            OpCode::BuildList => "BUILD_LIST",
        }
    }
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.mnemonic())
    }
}

/// 字节码指令
///
/// 指令由操作码和可选的操作数组成。
#[derive(Debug, Clone)]
pub struct Instruction {
    /// 操作码
    pub opcode: OpCode,

    /// 操作数（变长）
    pub operands: Vec<Operand>,
}

/// 指令操作数
#[derive(Debug, Clone)]
pub enum Operand {
    /// 8位无符号整数
    U8(u8),

    /// 16位无符号整数
    U16(u16),

    /// 32位无符号整数
    U32(u32),

    /// 64位浮点数
    F64(f64),

    /// 字节串（用于字符串常量等）
    Bytes(Vec<u8>),
}

impl Instruction {
    /// 创建新指令
    pub fn new(opcode: OpCode, operands: Vec<Operand>) -> Self {
        Self { opcode, operands }
    }

    /// 创建无操作数指令
    pub fn simple(opcode: OpCode) -> Self {
        Self {
            opcode,
            operands: Vec::new(),
        }
    }

    /// 编码为字节序列
    pub fn encode(&self) -> Vec<u8> {
        let mut bytes = vec![self.opcode as u8];

        for operand in &self.operands {
            match operand {
                Operand::U8(v) => bytes.push(*v),
                Operand::U16(v) => bytes.extend_from_slice(&v.to_le_bytes()),
                Operand::U32(v) => bytes.extend_from_slice(&v.to_le_bytes()),
                Operand::F64(v) => bytes.extend_from_slice(&v.to_le_bytes()),
                Operand::Bytes(b) => bytes.extend_from_slice(b),
            }
        }

        bytes
    }

    /// 从字节序列解码指令
    pub fn decode(bytes: &[u8], offset: &mut usize) -> Option<Self> {
        if *offset >= bytes.len() {
            return None;
        }

        let opcode_byte = bytes[*offset];
        *offset += 1;

        let opcode = OpCode::from_byte(opcode_byte)?;
        let mut operands = Vec::new();

        // 根据操作码解析操作数
        match opcode {
            OpCode::LoadConst | OpCode::LoadGlobal | OpCode::StoreGlobal | OpCode::MakeLambda | OpCode::LoadBuiltin => {
                // 单个 u32 操作数
                if *offset + 4 <= bytes.len() {
                    let mut arr = [0u8; 4];
                    arr.copy_from_slice(&bytes[*offset..*offset + 4]);
                    operands.push(Operand::U32(u32::from_le_bytes(arr)));
                    *offset += 4;
                }
            }

            OpCode::LoadLocal | OpCode::StoreLocal => {
                // 两个 u8 操作数：深度和偏移量
                if *offset + 2 <= bytes.len() {
                    operands.push(Operand::U8(bytes[*offset]));
                    operands.push(Operand::U8(bytes[*offset + 1]));
                    *offset += 2;
                }
            }

            OpCode::Jump | OpCode::JumpIfFalse | OpCode::JumpIfTrue => {
                // 跳转偏移量（i32）
                if *offset + 4 <= bytes.len() {
                    let mut arr = [0u8; 4];
                    arr.copy_from_slice(&bytes[*offset..*offset + 4]);
                    operands.push(Operand::U32(u32::from_le_bytes(arr)));
                    *offset += 4;
                }
            }

            OpCode::Call | OpCode::TailCall | OpCode::BuildList => {
                // 参数个数（u8）
                if *offset < bytes.len() {
                    operands.push(Operand::U8(bytes[*offset]));
                    *offset += 1;
                }
            }

            // 无操作数指令
            _ => {}
        }

        Some(Instruction { opcode, operands })
    }

    /// 格式化指令为可读字符串
    pub fn format(&self) -> String {
        let mut result = self.opcode.mnemonic().to_string();

        if !self.operands.is_empty() {
            result.push(' ');
            for (i, operand) in self.operands.iter().enumerate() {
                if i > 0 {
                    result.push_str(", ");
                }
                match operand {
                    Operand::U8(v) => result.push_str(&v.to_string()),
                    Operand::U16(v) => result.push_str(&v.to_string()),
                    Operand::U32(v) => result.push_str(&v.to_string()),
                    Operand::F64(v) => result.push_str(&format!("{:.2}", v)),
                    Operand::Bytes(b) => result.push_str(&format!("{:?}", b)),
                }
            }
        }

        result
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format())
    }
}

/// 字节码块
///
/// 包含一个完整的字节码函数或代码块。
#[derive(Debug, Clone)]
pub struct Chunk {
    /// 字节码指令序列
    pub code: Vec<u8>,

    /// 常量表
    pub constants: Vec<Constant>,

    /// 调试信息（行号等）
    pub debug_info: DebugInfo,
}

/// 常量值
#[derive(Debug, Clone)]
pub enum Constant {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    /// Lambda 函数
    Lambda {
        /// 参数列表
        params: Vec<String>,
        /// 函数体的字节码
        chunk: Chunk,
        /// 需要捕获的闭包变量（变量名）
        captures: Vec<String>,
    },
}

/// 调试信息
#[derive(Debug, Clone)]
pub struct DebugInfo {
    /// 每条字节码指令对应的源代码行号
    pub lines: Vec<usize>,
}

impl Chunk {
    /// 创建新的字节码块
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            debug_info: DebugInfo { lines: Vec::new() },
        }
    }

    /// 写入指令到字节码块
    pub fn write_instruction(&mut self, instruction: &Instruction, line: usize) {
        let bytes = instruction.encode();
        self.code.extend_from_slice(&bytes);
        self.debug_info.lines.push(line);
    }

    /// 添加常量并返回其索引
    pub fn add_constant(&mut self, constant: Constant) -> usize {
        // 检查是否已存在相同常量
        for (i, existing) in self.constants.iter().enumerate() {
            if Self::constants_equal(existing, &constant) {
                return i;
            }
        }

        self.constants.push(constant);
        self.constants.len() - 1
    }

    /// 比较两个常量是否相等
    fn constants_equal(a: &Constant, b: &Constant) -> bool {
        match (a, b) {
            (Constant::Number(x), Constant::Number(y)) => x == y,
            (Constant::String(x), Constant::String(y)) => x == y,
            (Constant::Boolean(x), Constant::Boolean(y)) => x == y,
            (Constant::Nil, Constant::Nil) => true,
            // Lambda 函数不进行去重（每个 lambda 都是唯一的）
            (Constant::Lambda { .. }, Constant::Lambda { .. }) => false,
            _ => false,
        }
    }

    /// 反汇编字节码块
    pub fn disassemble(&self, name: &str) -> String {
        let mut result = format!("== {} ({} bytes) ==\n", name, self.code.len());
        result.push_str(&format!("Constants: {}\n", self.constants.len()));

        for (i, constant) in self.constants.iter().enumerate() {
            result.push_str(&format!("  [{}] {:?}\n", i, constant));
        }

        result.push_str("\n");

        let mut offset = 0;
        while offset < self.code.len() {
            let line = if offset < self.debug_info.lines.len() {
                self.debug_info.lines[offset]
            } else {
                0
            };

            if let Some(instruction) = Instruction::decode(&self.code, &mut offset) {
                result.push_str(&format!(
                    "{:04} {:>20} | line {}\n",
                    offset - instruction.encode().len(),
                    instruction.format(),
                    line
                ));
            } else {
                result.push_str(&format!("{:04} ???\n", offset));
                offset += 1;
            }
        }

        result
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_opcode_from_byte() {
        assert_eq!(OpCode::from_byte(0), Some(OpCode::LoadConst));
        assert_eq!(OpCode::from_byte(20), Some(OpCode::Add));
        assert_eq!(OpCode::from_byte(255), None);
    }

    #[test]
    fn test_instruction_encode_decode() {
        let inst = Instruction::new(OpCode::Add, vec![]);
        let bytes = inst.encode();
        assert_eq!(bytes, vec![20]);

        let mut offset = 0;
        let decoded = Instruction::decode(&bytes, &mut offset).unwrap();
        assert_eq!(decoded.opcode, OpCode::Add);
        assert_eq!(decoded.operands.len(), 0);
    }

    #[test]
    fn test_instruction_with_operands() {
        let inst = Instruction::new(
            OpCode::LoadConst,
            vec![Operand::U32(42)],
        );

        let bytes = inst.encode();
        assert_eq!(bytes[0], OpCode::LoadConst as u8);

        let mut offset = 0;
        let decoded = Instruction::decode(&bytes, &mut offset).unwrap();
        assert_eq!(decoded.opcode, OpCode::LoadConst);
        assert_eq!(decoded.operands.len(), 1);
    }

    #[test]
    fn test_chunk_add_constant() {
        let mut chunk = Chunk::new();

        let idx1 = chunk.add_constant(Constant::Number(42.0));
        let idx2 = chunk.add_constant(Constant::Number(42.0)); // 应该返回相同索引
        let idx3 = chunk.add_constant(Constant::Number(43.0));

        assert_eq!(idx1, 0);
        assert_eq!(idx2, 0); // 相同常量，相同索引
        assert_eq!(idx3, 1);
        assert_eq!(chunk.constants.len(), 2);
    }

    #[test]
    fn test_chunk_disassemble() {
        let mut chunk = Chunk::new();
        chunk.add_constant(Constant::Number(42.0));

        let inst = Instruction::simple(OpCode::Add);
        chunk.write_instruction(&inst, 1);

        let disasm = chunk.disassemble("test");
        assert!(disasm.contains("test"));
        assert!(disasm.contains("ADD"));
    }
}
