;; Check whether literal integers are correctly signed.

(and (=  4294967295 (- (expt 2 32) 1))      ;; unsigned
     (= -2147483648 (- (expt 2 31)))        ;; signed
     (=  2147483648 (expt 2 31)))           ;; unsigned
