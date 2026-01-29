; 测试高阶列表函数

(display "=== Testing Higher-Order Functions ===")
(newline)

; 测试1: map
(display "Test 1 - map:")
(define nums (list 1 2 3 4 5))
(display "  Original: ")
(display nums)
(newline)
(display "  Squared: ")
(display (map (lambda (x) (* x x)) nums))
(newline)
(display "  Doubled: ")
(display (map (lambda (x) (* x 2)) nums))
(newline)

; 测试2: filter
(display "Test 2 - filter:")
(display "  Even numbers: ")
(display (filter (lambda (x) (= (mod x 2) 0)) nums))
(newline)
(display "  Greater than 3: ")
(display (filter (lambda (x) (> x 3)) nums))
(newline)

; 测试3: fold (reduce)
(display "Test 3 - fold:")
(display "  Sum: ")
(display (fold (lambda (acc x) (+ acc x)) 0 nums))
(newline)
(display "  Product: ")
(display (fold (lambda (acc x) (* acc x)) 1 nums))
(newline)
(display "  Max (custom): ")
(display (fold (lambda (acc x) (if (> x acc) x acc)) (head nums) nums))
(newline)

; 测试4: length
(display "Test 4 - length:")
(display "  Length of nums: ")
(display (length nums))
(newline)
(display "  Length of empty: ")
(display (length (list)))
(newline)

; 测试5: reverse
(display "Test 5 - reverse:")
(display "  Reversed nums: ")
(display (reverse nums))
(newline)

; 测试6: 组合使用
(display "Test 6 - Combining functions:")
(display "  Sum of squares of even numbers: ")
(display
  (fold (lambda (acc x) (+ acc x))
        0
        (map (lambda (x) (* x x))
             (filter (lambda (x) (= (mod x 2) 0))
                     nums))))
(newline)

; 测试7: 复杂数据处理
(display "Test 7 - Complex data processing:")
(define grades (list 85 92 78 95 88 76 89))
(display "  Grades: ")
(display grades)
(newline)

(display "  Passed (>80): ")
(display (length (filter (lambda (g) (> g 80)) grades)))
(newline)

(display "  Average: ")
(display
  (/ (fold (lambda (acc g) (+ acc g)) 0 grades)
     (length grades)))
(newline)

(display "  Curved grades (+5): ")
(display (map (lambda (g) (+ g 5)) grades))
(newline)

; 测试8: 使用 map 创建列表
(display "Test 8 - Creating lists with map:")
(display "  Range 1-10: ")
(display (map (lambda (x) (+ x 1)) (list 0 1 2 3 4 5 6 7 8 9)))
(newline)

(display "=== All higher-order function tests completed! ===")
(newline)
