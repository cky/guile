
(define income-tax
  (lambda (income)
    (cond
     ((<= income 10000)
      (* income .05))
     ((<= income 20000)
      (+ (* (- income 10000) .08)
	 500.00))
     ((<= income 30000)
      (+ (* (- income 20000) .13)
	 1300.00))
     (else
      (+ (* (- income 30000) .21)
	 2600.00)))))

(test (income-tax 5000) 250.0)
(test (income-tax 15000) 900.0)
(test (income-tax 25000) 1950.0)
(test (income-tax 50000) 6800.0)
