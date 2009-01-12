(let loop ((i 10000000))
  (and (> i 0)
       (loop (1- i))))

