//! FFI/syscall 支持

use crate::ast::Expr;
use libffi::middle::{Arg, Cif, CodePtr, Type};
use libloading::Library;
use std::ffi::CString;
use std::os::raw::c_void;

#[derive(Clone, Copy, Debug, PartialEq)]
enum FfiType {
    I64,
    U64,
    Isize,
    Usize,
    F64,
    Bool,
    Ptr,
    String,
    Bytes,
    Void,
}

#[derive(Debug)]
enum FfiValue {
    I64(i64),
    U64(u64),
    Isize(isize),
    Usize(usize),
    F64(f64),
    Bool(u8),
    Ptr(*mut c_void),
    CString(CString),
    Bytes(Vec<u8>),
}

impl FfiType {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let name = match expr {
            Expr::String(s) => s.as_str(),
            Expr::Symbol(s) => s.as_str(),
            _ => return Err("ffi-call: type must be a string or symbol".to_string()),
        };

        match name {
            "i64" => Ok(FfiType::I64),
            "u64" => Ok(FfiType::U64),
            "isize" => Ok(FfiType::Isize),
            "usize" => Ok(FfiType::Usize),
            "f64" => Ok(FfiType::F64),
            "bool" => Ok(FfiType::Bool),
            "ptr" => Ok(FfiType::Ptr),
            "string" => Ok(FfiType::String),
            "bytes" => Ok(FfiType::Bytes),
            "void" => Ok(FfiType::Void),
            _ => Err(format!("ffi-call: unknown type '{}'", name)),
        }
    }

    fn to_ffi_type(self) -> Type {
        match self {
            FfiType::I64 => Type::i64(),
            FfiType::U64 => Type::u64(),
            FfiType::Isize => Type::isize(),
            FfiType::Usize => Type::usize(),
            FfiType::F64 => Type::f64(),
            FfiType::Bool => Type::u8(),
            FfiType::Ptr | FfiType::String | FfiType::Bytes => Type::pointer(),
            FfiType::Void => Type::void(),
        }
    }
}

fn lib_name(input: &str) -> String {
    #[cfg(target_os = "windows")]
    {
        match input {
            "c" | "msvcrt" => "msvcrt.dll".to_string(),
            "kernel32" => "Kernel32.dll".to_string(),
            "ntdll" => "ntdll.dll".to_string(),
            _ => input.to_string(),
        }
    }

    #[cfg(target_os = "macos")]
    {
        match input {
            "c" => "libc.dylib".to_string(),
            "m" => "libm.dylib".to_string(),
            _ => input.to_string(),
        }
    }

    #[cfg(all(unix, not(target_os = "macos")))]
    {
        match input {
            "c" => "libc.so.6".to_string(),
            "m" => "libm.so.6".to_string(),
            _ => input.to_string(),
        }
    }
}

fn parse_number(expr: &Expr, context: &str) -> Result<f64, String> {
    match expr {
        Expr::Number(n) => Ok(*n),
        _ => Err(format!("{}: expected number", context)),
    }
}

fn parse_bool(expr: &Expr) -> bool {
    match expr {
        Expr::Bool(b) => *b,
        Expr::Nil => false,
        _ => true,
    }
}

fn parse_bytes(expr: &Expr) -> Result<Vec<u8>, String> {
    match expr {
        Expr::List(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                let n = parse_number(item, "ffi-call: bytes")?;
                out.push(n as u8);
            }
            Ok(out)
        }
        _ => Err("ffi-call: bytes must be a list of numbers".to_string()),
    }
}

fn value_from_expr(expr: &Expr, ty: FfiType) -> Result<FfiValue, String> {
    match ty {
        FfiType::I64 => Ok(FfiValue::I64(parse_number(expr, "ffi-call")? as i64)),
        FfiType::U64 => Ok(FfiValue::U64(parse_number(expr, "ffi-call")? as u64)),
        FfiType::Isize => Ok(FfiValue::Isize(parse_number(expr, "ffi-call")? as isize)),
        FfiType::Usize => Ok(FfiValue::Usize(parse_number(expr, "ffi-call")? as usize)),
        FfiType::F64 => Ok(FfiValue::F64(parse_number(expr, "ffi-call")?)),
        FfiType::Bool => Ok(FfiValue::Bool(if parse_bool(expr) { 1 } else { 0 })),
        FfiType::Ptr => Ok(FfiValue::Ptr(parse_number(expr, "ffi-call")? as usize as *mut c_void)),
        FfiType::String => match expr {
            Expr::String(s) => Ok(FfiValue::CString(CString::new(s.as_str()).map_err(|_| "ffi-call: invalid string".to_string())?)),
            Expr::Symbol(s) => Ok(FfiValue::CString(CString::new(s.as_str()).map_err(|_| "ffi-call: invalid string".to_string())?)),
            _ => Err("ffi-call: string argument must be string or symbol".to_string()),
        },
        FfiType::Bytes => Ok(FfiValue::Bytes(parse_bytes(expr)?)),
        FfiType::Void => Err("ffi-call: void is not valid for argument type".to_string()),
    }
}

pub fn apply_ffi_call(args: &[Expr]) -> Result<Expr, String> {
    if args.len() < 4 {
        return Err("ffi-call requires at least 4 arguments".to_string());
    }

    let lib = match &args[0] {
        Expr::String(s) => s.as_str(),
        Expr::Symbol(s) => s.as_str(),
        _ => return Err("ffi-call: lib must be string or symbol".to_string()),
    };

    let func = match &args[1] {
        Expr::String(s) => s.as_str(),
        Expr::Symbol(s) => s.as_str(),
        _ => return Err("ffi-call: function name must be string or symbol".to_string()),
    };

    let ret_type = FfiType::from_expr(&args[2])?;
    if ret_type == FfiType::String || ret_type == FfiType::Bytes {
        return Err("ffi-call: string/bytes return not supported; use ptr".to_string());
    }

    let arg_types = match &args[3] {
        Expr::List(list) => list,
        _ => return Err("ffi-call: arg types must be a list".to_string()),
    };

    let provided_args = &args[4..];
    if arg_types.len() != provided_args.len() {
        return Err(format!(
            "ffi-call: arg count mismatch, expected {}, got {}",
            arg_types.len(),
            provided_args.len()
        ));
    }

    let mut ffi_types = Vec::with_capacity(arg_types.len());
    let mut values = Vec::with_capacity(arg_types.len());
    for (ty_expr, arg_expr) in arg_types.iter().zip(provided_args.iter()) {
        let ty = FfiType::from_expr(ty_expr)?;
        ffi_types.push(ty.to_ffi_type());
        values.push(value_from_expr(arg_expr, ty)?);
    }

    let lib_path = lib_name(lib);
    let library = unsafe { Library::new(lib_path) }
        .map_err(|e| format!("ffi-call: failed to load lib: {}", e))?;

    let symbol = unsafe {
        library
            .get::<*mut c_void>(func.as_bytes())
            .map_err(|e| format!("ffi-call: failed to load symbol: {}", e))?
    };

    let cif = Cif::new(ffi_types, ret_type.to_ffi_type());
    let code_ptr = CodePtr::from_ptr(*symbol);

    let mut keep_cstrings: Vec<CString> = Vec::new();
    let mut keep_bytes: Vec<Vec<u8>> = Vec::new();
    let mut keep_ptrs: Vec<*mut c_void> = Vec::new();
    let mut args_vec: Vec<Arg> = Vec::with_capacity(values.len());

    for value in &values {
        match value {
            FfiValue::I64(v) => args_vec.push(Arg::new(v)),
            FfiValue::U64(v) => args_vec.push(Arg::new(v)),
            FfiValue::Isize(v) => args_vec.push(Arg::new(v)),
            FfiValue::Usize(v) => args_vec.push(Arg::new(v)),
            FfiValue::F64(v) => args_vec.push(Arg::new(v)),
            FfiValue::Bool(v) => args_vec.push(Arg::new(v)),
            FfiValue::Ptr(p) => args_vec.push(Arg::new(p)),
            FfiValue::CString(s) => {
                keep_cstrings.push(s.clone());
                let ptr = keep_cstrings.last().unwrap().as_ptr() as *mut c_void;
                keep_ptrs.push(ptr);
                args_vec.push(Arg::new(keep_ptrs.last().unwrap()));
            }
            FfiValue::Bytes(bytes) => {
                keep_bytes.push(bytes.clone());
                let ptr = keep_bytes.last().unwrap().as_ptr() as *mut c_void;
                keep_ptrs.push(ptr);
                args_vec.push(Arg::new(keep_ptrs.last().unwrap()));
            }
        }
    }

    let result = unsafe {
        match ret_type {
            FfiType::Void => {
                cif.call::<()>(code_ptr, &args_vec);
                Expr::Nil
            }
            FfiType::I64 => Expr::Number(cif.call::<i64>(code_ptr, &args_vec) as f64),
            FfiType::U64 => Expr::Number(cif.call::<u64>(code_ptr, &args_vec) as f64),
            FfiType::Isize => Expr::Number(cif.call::<isize>(code_ptr, &args_vec) as f64),
            FfiType::Usize => Expr::Number(cif.call::<usize>(code_ptr, &args_vec) as f64),
            FfiType::F64 => Expr::Number(cif.call::<f64>(code_ptr, &args_vec)),
            FfiType::Bool => Expr::Bool(cif.call::<u8>(code_ptr, &args_vec) != 0),
            FfiType::Ptr => Expr::Number(cif.call::<*mut c_void>(code_ptr, &args_vec) as usize as f64),
            FfiType::String | FfiType::Bytes => Expr::Nil,
        }
    };

    drop(library);
    Ok(result)
}

pub fn apply_syscall(args: &[Expr]) -> Result<Expr, String> {
    if args.is_empty() {
        return Err("syscall requires at least 1 argument".to_string());
    }

    #[cfg(target_os = "windows")]
    {
        let _ = args;
        return Err("syscall not supported on windows".to_string());
    }

    #[cfg(not(target_os = "windows"))]
    unsafe {
        let num = parse_number(&args[0], "syscall")? as libc::c_long;
        let mut raw_args: Vec<libc::c_long> = Vec::new();
        for arg in &args[1..] {
            raw_args.push(parse_number(arg, "syscall")? as libc::c_long);
        }

        let result = match raw_args.len() {
            0 => libc::syscall(num),
            1 => libc::syscall(num, raw_args[0]),
            2 => libc::syscall(num, raw_args[0], raw_args[1]),
            3 => libc::syscall(num, raw_args[0], raw_args[1], raw_args[2]),
            4 => libc::syscall(num, raw_args[0], raw_args[1], raw_args[2], raw_args[3]),
            5 => libc::syscall(num, raw_args[0], raw_args[1], raw_args[2], raw_args[3], raw_args[4]),
            6 => libc::syscall(num, raw_args[0], raw_args[1], raw_args[2], raw_args[3], raw_args[4], raw_args[5]),
            _ => return Err("syscall supports up to 6 arguments".to_string()),
        };

        Ok(Expr::Number(result as f64))
    }
}
