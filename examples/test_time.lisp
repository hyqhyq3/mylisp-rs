;; MyLisp 时间库测试

;; 加载时间库
(load stdlib/time.lisp)

(display "========================================")
(newline)
(display "  MyLisp 标准库 - 时间模块测试")
(newline)
(display "========================================")
(newline)
(newline)

;; 运行测试
(time-test)

(display "========================================")
(newline)
(display "  测试完成！")
(newline)
(display "========================================")
(newline)
