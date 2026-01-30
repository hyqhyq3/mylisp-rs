#!/bin/bash
# MyLisp 性能基准测试脚本

set -e

echo "==================================="
echo "MyLisp 性能基准测试"
echo "==================================="
echo ""

# 检查是否安装了必要的工具
if ! command -v cargo &> /dev/null; then
    echo "错误: 未找到 cargo，请先安装 Rust"
    exit 1
fi

# 解析命令行参数
BENCH_TYPE="${1:-quick}"

case "$BENCH_TYPE" in
    quick)
        echo "📊 运行快速基准测试 (约30秒)..."
        echo ""
        cargo bench --bench quick_bench
        ;;

    standard)
        echo "📊 运行标准基准测试 (约2-3分钟)..."
        echo ""
        cargo bench --bench benchmark
        ;;

    full)
        echo "📊 运行完整基准测试 (包含所有文件加载，约5分钟)..."
        echo ""
        cargo bench --bench benchmark -- --sample-size 50
        ;;

    flamegraph)
        echo "🔥 生成性能火焰图..."
        echo ""
        cargo bench --bench flamegraph
        echo ""
        echo "火焰图已保存到: target/flamegraph/"
        ;;

    specific)
        if [ -z "$2" ]; then
            echo "用法: $0 specific <benchmark_name>"
            echo "示例: $0 specific fibonacci"
            exit 1
        fi
        echo "📊 运行特定基准测试: $2"
        echo ""
        cargo bench --bench benchmark -- "$2"
        ;;

    baseline)
        echo "📊 创建性能基线..."
        echo ""
        cargo bench --bench benchmark -- --save-baseline main
        echo ""
        echo "✅ 基线已保存！后续可以使用以下命令对比："
        echo "   cargo bench -- --baseline main"
        ;;

    compare)
        if [ -z "$2" ]; then
            echo "用法: $0 compare <baseline_name>"
            echo "示例: $0 compare main"
            exit 1
        fi
        echo "📊 与基线 '$2' 对比..."
        echo ""
        cargo bench --bench benchmark -- --baseline "$2"
        ;;

    *)
        echo "用法: $0 [quick|standard|full|flamegraph|specific|baseline|compare]"
        echo ""
        echo "选项:"
        echo "  quick      - 快速测试 (默认，约30秒)"
        echo "  standard   - 标准测试 (约2-3分钟)"
        echo "  full       - 完整测试 (约5分钟)"
        echo "  flamegraph - 生成性能火焰图"
        echo "  specific   - 运行特定测试 (需要指定测试名)"
        echo "  baseline   - 创建性能基线"
        echo "  compare    - 与基线对比"
        echo ""
        echo "示例:"
        echo "  $0 quick              # 快速测试"
        echo "  $0 specific fibonacci # 只测试斐波那契"
        echo "  $0 baseline           # 创建基线"
        echo "  $0 compare main       # 与基线对比"
        exit 1
        ;;
esac

echo ""
echo "==================================="
echo "✅ 测试完成！"
echo ""
echo "查看详细报告: target/criterion/report/index.html"
echo "==================================="
