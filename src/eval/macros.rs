//! 宏系统模块
//!
//! 实现基于 syntax-rules 的宏系统，支持模式匹配和模板展开

use crate::ast::Expr;
use crate::eval::error::MyLispError;
use std::collections::HashMap;

/// 宏定义：使用 syntax-rules
#[derive(Debug, Clone)]
pub struct Macro {
    pub name: String,
    pub rules: Vec<MacroRule>,
}

#[derive(Debug, Clone)]
pub struct MacroRule {
    pub pattern: Expr,     // 模式
    pub template: Expr,    // 输出模板
    pub literals: Vec<String>,  // 字面量标识符
}

/// 模式变量绑定：支持多个值的列表（用于 ...）
#[derive(Debug, Clone)]
pub enum PatternBinding {
    Single(Expr),
    Multiple(Vec<Expr>),
}

/// 全局宏注册表
thread_local! {
    static MACROS: RefCell<std::collections::HashMap<String, Macro>> =
        RefCell::new(std::collections::HashMap::new());
}

use std::cell::RefCell;

/// 注册宏
pub fn register_macro(name: String, macro_def: Macro) {
    MACROS.with(|m| m.borrow_mut().insert(name, macro_def));
}

/// 查找宏
pub fn lookup_macro(name: &str) -> Option<Macro> {
    MACROS.with(|m| m.borrow().get(name).cloned())
}

/// 宏系统应用器
pub struct MacroSystem;

impl MacroSystem {
    /// 展开表达式中的所有宏
    pub fn expand_macros(expr: Expr) -> Result<Expr, MyLispError> {
        match expr {
            Expr::List(list) if !list.is_empty() => {
                // 检查是否是宏调用
                match &list[0] {
                    Expr::Symbol(sym) => {
                        if let Some(macro_def) = lookup_macro(sym) {
                            // 展开宏
                            let expanded = Self::apply_macro(&macro_def, &list)?;
                            // 递归展开结果
                            return Self::expand_macros(expanded);
                        }
                    }
                    _ => {}
                }

                // 递归展开列表元素
                let expanded: Result<Vec<Expr>, MyLispError> = list
                    .into_iter()
                    .map(|e| Self::expand_macros(e))
                    .collect();
                Ok(Expr::List(expanded?))
            }
            _ => Ok(expr),
        }
    }

    /// 应用宏：模式匹配和模板展开
    fn apply_macro(macro_def: &Macro, args: &[Expr]) -> Result<Expr, MyLispError> {
        for rule in &macro_def.rules {
            if let Some(binding) = Self::match_pattern(&rule.pattern, args, &rule.literals) {
                return Self::expand_template(&rule.template, &binding);
            }
        }
        Err(MyLispError::runtime(format!(
            "Macro '{}' pattern match failed",
            macro_def.name
        )))
    }

    // ========== 模式匹配 ==========

    /// 模式匹配：返回模式变量绑定（支持 ...）
    fn match_pattern(
        pattern: &Expr,
        expr: &[Expr],
        literals: &[String],
    ) -> Option<HashMap<String, PatternBinding>> {
        if expr.is_empty() {
            return None;
        }

        // 检查操作符是否匹配
        let pattern_list = match pattern {
            Expr::List(l) => l.as_slice(),
            _ => return None,
        };

        if pattern_list.is_empty() {
            return None;
        }

        let pattern_op = match &pattern_list[0] {
            Expr::Symbol(s) => s.clone(),
            _ => return None,
        };

        let expr_op = match &expr[0] {
            Expr::Symbol(s) => s.clone(),
            _ => return None,
        };

        if pattern_op != expr_op {
            return None;
        }

        let mut bindings = HashMap::new();

        // 匹配参数（支持 ...）
        Self::match_pattern_list(&pattern_list[1..], &expr[1..], literals, &mut bindings)
            .then_some(bindings)
    }

    /// 匹配模式列表，支持省略号
    fn match_pattern_list(
        pattern: &[Expr],
        expr: &[Expr],
        literals: &[String],
        bindings: &mut HashMap<String, PatternBinding>,
    ) -> bool {
        let mut p_idx = 0;
        let mut e_idx = 0;

        while p_idx < pattern.len() && e_idx < expr.len() {
            // 检查是否是 ... 模式
            let has_ellipsis = p_idx + 1 < pattern.len()
                && matches!(&pattern[p_idx + 1], Expr::Symbol(s) if s == "...");

            if has_ellipsis {
                // 处理可变参数模式
                match &pattern[p_idx] {
                    Expr::Symbol(sym) if !literals.contains(sym) && sym != "..." => {
                        // 收集剩余所有表达式
                        let mut collected = Vec::new();
                        while e_idx < expr.len() {
                            collected.push(expr[e_idx].clone());
                            e_idx += 1;
                        }
                        bindings.insert(sym.clone(), PatternBinding::Multiple(collected));
                        p_idx += 2; // 跳过模式和 ...
                        break;
                    }
                    _ => return false,
                }
            } else {
                // 正常匹配单个元素
                if !Self::match_pattern_single(&pattern[p_idx], &expr[e_idx], literals, bindings) {
                    return false;
                }
                p_idx += 1;
                e_idx += 1;
            }
        }

        // 检查是否所有模式都匹配
        while p_idx < pattern.len() {
            let has_ellipsis = p_idx + 1 < pattern.len()
                && matches!(&pattern[p_idx + 1], Expr::Symbol(s) if s == "...");

            if has_ellipsis {
                // 空列表匹配
                match &pattern[p_idx] {
                    Expr::Symbol(sym) if !literals.contains(sym) && sym != "..." => {
                        bindings.insert(sym.clone(), PatternBinding::Multiple(vec![]));
                        p_idx += 2;
                    }
                    _ => return false,
                }
            } else {
                return false;
            }
        }

        e_idx == expr.len()
    }

    /// 单个模式元素匹配
    fn match_pattern_single(
        pattern: &Expr,
        expr: &Expr,
        literals: &[String],
        bindings: &mut HashMap<String, PatternBinding>,
    ) -> bool {
        match pattern {
            Expr::Symbol(sym) => {
                // 检查是否是字面量
                if literals.contains(sym) {
                    match expr {
                        Expr::Symbol(s) if s == sym => true,
                        _ => false,
                    }
                } else if sym == "..." {
                    // 省略号不应该在这里单独出现
                    false
                } else {
                    // 模式变量
                    bindings.insert(sym.clone(), PatternBinding::Single(expr.clone()));
                    true
                }
            }
            Expr::List(pattern_list) => match expr {
                Expr::List(expr_list) => {
                    Self::match_pattern_list(pattern_list, expr_list, literals, bindings)
                }
                _ => false,
            },
            _ => pattern == expr,
        }
    }

    // ========== 模板展开 ==========

    /// 模板展开（支持 ... 重复）
    fn expand_template(
        template: &Expr,
        bindings: &HashMap<String, PatternBinding>,
    ) -> Result<Expr, MyLispError> {
        match template {
            Expr::Symbol(sym) => {
                if let Some(binding) = bindings.get(sym) {
                    match binding {
                        PatternBinding::Single(expr) => Ok(expr.clone()),
                        PatternBinding::Multiple(list) => Ok(Expr::List(list.clone())),
                    }
                } else {
                    Ok(Expr::Symbol(sym.clone()))
                }
            }
            Expr::List(list) => {
                // 检查是否有省略号模式
                Self::expand_template_with_ellipsis(list, bindings)
            }
            _ => Ok(template.clone()),
        }
    }

    /// 展开模板列表，处理省略号重复
    fn expand_template_with_ellipsis(
        template: &[Expr],
        bindings: &HashMap<String, PatternBinding>,
    ) -> Result<Expr, MyLispError> {
        let mut result = Vec::new();
        let mut i = 0;

        while i < template.len() {
            // 检查是否后面跟着省略号
            let has_ellipsis = i + 1 < template.len()
                && matches!(&template[i + 1], Expr::Symbol(s) if s == "...");

            if has_ellipsis {
                // 处理重复模式
                match &template[i] {
                    Expr::Symbol(sym) => {
                        if let Some(binding) = bindings.get(sym) {
                            match binding {
                                PatternBinding::Single(expr) => {
                                    result.push(expr.clone());
                                }
                                PatternBinding::Multiple(list) => {
                                    // 重复展开列表中的每个元素
                                    for item in list {
                                        result.push(item.clone());
                                    }
                                }
                            }
                        } else {
                            // 未绑定的变量，保持原样
                            result.push(template[i].clone());
                        }
                    }
                    Expr::List(sub_template) => {
                        // 检查是否需要嵌套展开
                        if let Some(repeated_bindings) =
                            Self::extract_repeated_bindings(sub_template, bindings)
                        {
                            // 为每个重复值展开模板
                            for single_bindings in repeated_bindings {
                                let expanded =
                                    Self::expand_template_list(sub_template, &single_bindings)?;
                                result.extend(expanded);
                            }
                        } else {
                            // 正常展开
                            let expanded = Self::expand_template_list(sub_template, bindings)?;
                            result.extend(expanded);
                        }
                    }
                    _ => {
                        result.push(template[i].clone());
                    }
                }
                i += 2; // 跳过模式和 ...
            } else {
                // 正常展开单个元素
                let expanded = Self::expand_template(&template[i], bindings)?;
                result.push(expanded);
                i += 1;
            }
        }

        Ok(Expr::List(result))
    }

    /// 展开模板列表（不处理省略号）
    fn expand_template_list(
        template: &[Expr],
        bindings: &HashMap<String, PatternBinding>,
    ) -> Result<Vec<Expr>, MyLispError> {
        template
            .iter()
            .map(|e| Self::expand_template(e, bindings))
            .collect()
    }

    /// 提取重复绑定（用于嵌套模板）
    fn extract_repeated_bindings(
        template: &[Expr],
        bindings: &HashMap<String, PatternBinding>,
    ) -> Option<Vec<HashMap<String, PatternBinding>>> {
        // 找出所有 Multiple 绑定
        let mut repeated_keys = Vec::new();
        for expr in template {
            if let Expr::Symbol(sym) = expr {
                if let Some(PatternBinding::Multiple(_)) = bindings.get(sym) {
                    repeated_keys.push(sym.clone());
                }
            }
        }

        if repeated_keys.is_empty() {
            return None;
        }

        // 获取第一个 Multiple 绑定的长度
        let first_len = match bindings.get(&repeated_keys[0]) {
            Some(PatternBinding::Multiple(list)) => list.len(),
            _ => return None,
        };

        // 为每个位置创建单独的绑定
        let mut result = Vec::new();
        for i in 0..first_len {
            let mut single_bindings = HashMap::new();

            for key in &repeated_keys {
                if let Some(PatternBinding::Multiple(list)) = bindings.get(key) {
                    if i < list.len() {
                        single_bindings.insert(
                            key.clone(),
                            PatternBinding::Single(list[i].clone()),
                        );
                    }
                }
            }

            // 复制非重复的绑定
            for (k, v) in bindings.iter() {
                if !repeated_keys.contains(k) {
                    single_bindings.insert(k.clone(), v.clone());
                }
            }

            result.push(single_bindings);
        }

        Some(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_macro_registration() {
        let macro_def = Macro {
            name: "test-macro".to_string(),
            rules: vec![],
        };

        register_macro("test-macro".to_string(), macro_def);
        let found = lookup_macro("test-macro");
        assert!(found.is_some());
        assert_eq!(found.unwrap().name, "test-macro");
    }

    #[test]
    fn test_pattern_binding() {
        let single = PatternBinding::Single(Expr::Number(42.0));
        let multiple = PatternBinding::Multiple(vec![
            Expr::Number(1.0),
            Expr::Number(2.0),
        ]);

        match single {
            PatternBinding::Single(Expr::Number(n)) => assert_eq!(n, 42.0),
            _ => panic!("Expected Single"),
        }

        match multiple {
            PatternBinding::Multiple(list) => assert_eq!(list.len(), 2),
            _ => panic!("Expected Multiple"),
        }
    }
}
