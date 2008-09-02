(list
  `()
  `foo
  `(foo)
  `(foo bar)
  `(1 2)
  (let ((x 1)) `,x)
  (let ((x 1)) `(,x))
  (let ((x 1)) ``(,x)))
