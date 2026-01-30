; FFI tests

(display "FFI Tests")
(newline)

; strlen("hello") -> 5
(display "Test 1 - strlen: ")
(display (ffi-call "c" "strlen" "usize" (list "string") "hello"))
(newline)

; cos(0.0) -> 1.0 (may be in libc on some platforms)
; Use libm on unix, msvcrt on windows
(display "Test 2 - cos: ")
(display (ffi-call "m" "cos" "f64" (list "f64") 0.0))
(newline)

(display "All FFI tests completed!")
(newline)
