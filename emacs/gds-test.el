
;; Test utility code.
(defun gds-test-execute-keys (keys &optional keys2)
  (execute-kbd-macro (apply 'vector (listify-key-sequence keys))))

(defvar gds-test-expecting nil)

(defun gds-test-protocol-hook (form)
  (message "[protocol: %s]" (car form))
  (if (eq (car form) gds-test-expecting)
      (setq gds-test-expecting nil)))

(defun gds-test-expect-protocol (proc &optional timeout)
  (message "[expect: %s]" proc)
  (setq gds-test-expecting proc)
  (while gds-test-expecting
    (or (accept-process-output gds-debug-server (or timeout 5))
	(error "Timed out after %ds waiting for %s" (or timeout 5) proc))))

(defun gds-test-check-buffer (name &rest strings)
  (let ((buf (or (get-buffer name) (error "No %s buffer" name))))
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (while strings
	(search-forward (car strings))
	(setq strings (cdr strings))))))

(defun TEST (desc)
  (message "TEST: %s" desc))

;; Make sure we take GDS elisp code from this code tree.
(setq load-path (cons (concat default-directory "emacs/") load-path))

;; Protect the tests so we can do some cleanups in case of error.
(unwind-protect
    (progn

      ;; Visit the tutorial.
      (find-file "gds-tutorial.txt")

      (TEST "Load up GDS.")
      (search-forward "(require 'gds)")
      (setq load-path (cons (concat default-directory "emacs/") load-path))
      (gds-test-execute-keys "\C-x\C-e")

      ;; Install our testing hook.
      (add-hook 'gds-protocol-hook 'gds-test-protocol-hook)

      (TEST "Help.")
      (search-forward "(list-ref")
      (backward-char 2)
      (gds-test-execute-keys "\C-hg\C-m")
      (gds-test-expect-protocol 'eval-results 10)
      (gds-test-check-buffer "*Guile Help*"
			     "help list-ref"
			     "is a primitive procedure in the (guile) module")

      (TEST "Completion.")
      (re-search-forward "^with-output-to-s")
      (gds-test-execute-keys "\e\C-i")
      (beginning-of-line)
      (or (looking-at "with-output-to-string")
	  (error "Expected completion `with-output-to-string' failed"))

      (TEST "Eval defun.")
      (search-forward "(display z)")
      (gds-test-execute-keys "\e\C-x")
      (gds-test-expect-protocol 'eval-results)
      (gds-test-check-buffer "*Guile Evaluation*"
			     "(let ((x 1) (y 2))"
			     "Arctangent is: 0.46"
			     "=> 0.46")

      (TEST "Multiple values.")
      (search-forward "(values 'a ")
      (gds-test-execute-keys "\e\C-x")
      (gds-test-expect-protocol 'eval-results)
      (gds-test-check-buffer "*Guile Evaluation*"
			     "(values 'a"
			     "hello world"
			     "=> a"
			     "=> b"
			     "=> c")

      (TEST "Eval region with multiple expressions.")
      (search-forward "(display \"Arctangent is: \")")
      (beginning-of-line)
      (push-mark nil nil t)
      (forward-line 3)
      (gds-test-execute-keys "\C-c\C-r")
      (gds-test-expect-protocol 'eval-results)
      (gds-test-check-buffer "*Guile Evaluation*"
			     "(display \"Arctangent is"
			     "Arctangent is:"
			     "=> no (or unspecified) value"
			     "ERROR: Unbound variable: z"
			     "=> error-in-evaluation"
			     "Evaluating expression 3"
			     "=> no (or unspecified) value")

      (TEST "Eval syntactically unbalanced region.")
      (search-forward "(let ((z (atan x y)))")
      (beginning-of-line)
      (push-mark nil nil t)
      (forward-line 4)
      (gds-test-execute-keys "\C-c\C-r")
      (gds-test-expect-protocol 'eval-results)
      (gds-test-check-buffer "*Guile Evaluation*"
			     "(let ((z (atan"
			     "Reading expressions to evaluate"
			     "ERROR"
			     "end of file"
			     "=> error-in-read")

      (TEST "Stepping through an evaluation.")
      (search-forward "(for-each (lambda (x)")
      (forward-line 1)
      (push-mark nil nil t)
      (forward-line 1)
      (gds-test-execute-keys "\C-u\e\C-x")
      (gds-test-expect-protocol 'stack)
      (gds-test-execute-keys " ")
      (gds-test-expect-protocol 'stack)
      (gds-test-execute-keys "o")
      (gds-test-expect-protocol 'stack)
      (gds-test-execute-keys "o")
      (gds-test-expect-protocol 'stack)
      (gds-test-execute-keys "o")
      (gds-test-expect-protocol 'stack)
      (gds-test-execute-keys "o")
      (gds-test-expect-protocol 'stack)
      (gds-test-execute-keys "o")
      (gds-test-expect-protocol 'stack)
      (gds-test-execute-keys "o")
      (gds-test-expect-protocol 'stack)
      (gds-test-execute-keys "o")
      (gds-test-expect-protocol 'stack)
      (gds-test-execute-keys "o")
      (gds-test-expect-protocol 'stack)
      (gds-test-execute-keys "o")
      (gds-test-expect-protocol 'stack)
      (gds-test-execute-keys "o")
      (gds-test-expect-protocol 'stack)
      (gds-test-execute-keys "g")
      (gds-test-expect-protocol 'eval-results)
      (gds-test-check-buffer "*Guile Evaluation*"
			     "(for-each (lambda"
			     "Evaluating in current module"
			     "3 cubed is 27"
			     "=> no (or unspecified) value")

      ;; Done.
      (message "====================================")
      (message "gds-test.el completed without errors")
      (message "====================================")
      
      )

  (switch-to-buffer "gds-debug")
  (write-region (point-min) (point-max) "gds-test.debug")

  (switch-to-buffer "*GDS Transcript*")
  (write-region (point-min) (point-max) "gds-test.transcript")

  )
