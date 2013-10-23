
; applicative-order evaluation
;(gcb 206 40)
;(gcb 40 6)    1 times
;(gcb 6 4)     2 times
;(gcb 4 2)     3 times
;(gcb 2 0)     4 times
;2

; normal-order evaluation
;(gcb 206 40)
;(gcb 40 (remainder 206 40)
;(gcb 40 6)    1 times
;(gcb 6 (remainder 40 6))
;(gcb 6 4)     2 times
;(gcb 4 (remainder 6 4))
;(gcb 4 2)     3 times
;(gcb 2 (remainder 4 2))
;(gcb 2 0)     4 times
;2
