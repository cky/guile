;;; "nclients.scm" Interface to net-client programs.
; Copyright 1997, 1998 Aubrey Jaffer
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(require 'string-search)
(require 'line-i/o)
(require 'system)
(require 'printf)
(require 'scanf)

;;@args proc
;;@args proc k
;;Calls @1 with @var{k} arguments, strings returned by successive
;;calls to @code{tmpnam}.  If @1 returns, then any files named by the
;;arguments to @1 are deleted automatically and the value(s) yielded
;;by the @1 is(are) returned.  @var{k} may be ommited, in which case
;;it defaults to @code{1}.
(define (call-with-tmpnam proc . k)
  (do ((cnt (if (null? k) 0 (+ -1 (car k))) (+ -1 cnt))
       (paths '() (cons (tmpnam) paths)))
      ((negative? cnt)
       (let ((ans (apply proc paths)))
	 (for-each (lambda (path) (if (file-exists? path) (delete-file path)))
		   paths)
	 ans))))

;;@args
;;@0 returns a string of the form @samp{username@r{@@}hostname}.  If
;;this e-mail address cannot be obtained, #f is returned.
(define user-email-address
  (let ((user (or (getenv "USER") (getenv "USERNAME")))
	(hostname (getenv "HOSTNAME")))	;with domain
    (lambda ()
      (if (not (and user hostname))
	  (call-with-tmpnam
	   (lambda (tmp)
	     (define command->string
	       (lambda (command)
		 (and (zero? (system (string-append command " >" tmp)))
		      (file-exists? tmp)
		      (let ((res #f))
			(call-with-input-file tmp
			  (lambda (port)
			    (and (eqv? 1 (fscanf port "%s" res)) res)))))))
	     (case (software-type)
	       ;;((AMIGA)				)
	       ;;((MACOS THINKC)			)
	       ((MS-DOS WINDOWS OS/2 ATARIST)
		(let ((compname (getenv "COMPUTERNAME")) ;without domain
		      (workgroup #f)
		      (netdir (or (getenv "windir")
				  (getenv "winbootdir")
				  (and (getenv "SYSTEMROOT")
				       (string-append (getenv "SYSTEMROOT")
						      "\\system32"))
				  "C:\\windows")))
		  (define (net . cmd)
		    (zero? (system (apply string-append
					  (or netdir "")
					  (if netdir "\\" "")
					  "NET " cmd))))
		  (and (not (and user hostname))
		       (zero? (system (string-append
				       (or netdir "")
				       (if netdir "\\" "")
				       "IPCONFIG /ALL > " tmp " ")))
		       (file-exists? tmp)
		       ;;(print tmp '=) (display-file tmp)
		       (call-with-input-file tmp
			 (lambda (port)
			   (find-string-from-port? "Host Name" port)
			   (fscanf port " %*[. ]: %s" hostname)
			   (delete-file tmp))))
		  (and (not (and user hostname))
		       (net "START /LIST >" tmp)
		       (file-exists? tmp)
		       (not (eof-object? (call-with-input-file tmp read-char)))
		       (cond
			((call-with-input-file tmp
			   (lambda (port)
			     (find-string-from-port? "o network servic" port)))
			 (and (net "CONFIG /YES >" tmp)
			      (net "STOP /YES")))
			(else (net "CONFIG /YES >" tmp)))
		       (call-with-input-file tmp
			 (lambda (port)
			   (do ((line (read-line port) (read-line port)))
			       ((eof-object? line))
			     (sscanf line " Workstation root directory %s"
				     netdir)
			     (sscanf line " Computer name \\\\%s" compname)
			     (sscanf line " Workstation Domain %s" workgroup)
			     (sscanf line " Workgroup %s" workgroup)
			     (sscanf line " User name %s" user)))))
		  (and netdir (not (and user hostname))
		       (set! netdir (string-append netdir "\\system.ini"))
		       (file-exists? netdir)
		       (call-with-input-file netdir
			 (lambda (port)
			   (and (find-string-from-port? "[DNS]" port)
				(read-line port) ;past newline
				(do ((line (read-line port) (read-line port)))
				    ((not (and (string? line)
					       (string-index line #\=))))
				  (sscanf line "HostName=%s" compname)
				  (sscanf line "DomainName=%s" workgroup)))))
		       (not user)
		       (call-with-input-file netdir
			 (lambda (port)
			   (and (find-string-from-port? "[Network]" port)
				(read-line port) ;past newline
				(do ((line (read-line port) (read-line port)))
				    ((not (and (string? line)
					       (string-index line #\=))))
				  (sscanf line "UserName=%s" user))))))
		  (if (and compname (not hostname))
		      (set! hostname
			    (string-append
			     compname "." (or workgroup "localnet"))))))
	       ;;((NOSVE)				)
	       ;;((VMS)					)
	       ((UNIX COHERENT)
		(if (not user)
		    (set! user (command->string "whoami")))
		(if (not hostname)
		    (set! hostname (command->string "hostname")))))
	     (if (not user) (set! user "John_Doe"))
	     (if (not hostname) (set! hostname "localhost")))))
      (string-append user "@" hostname))))

;;@args
;;@0 returns a string containing the absolute file name representing
;;the current working directory.  If this string cannot be obtained,
;;#f is returned.
;;
;;If @0 cannot be supported by the platform, the value of @0 is
;;#f.
(define current-directory
  (case (software-type)
    ;;((AMIGA)				)
    ;;((MACOS THINKC)			)
    ((MS-DOS WINDOWS ATARIST OS/2)
     (lambda ()
       (call-with-tmpnam
	(lambda (tmp)
	  (and (zero? (system (string-append "cd >" tmp)))
	       (file-exists? tmp)
	       (call-with-input-file tmp
		 (lambda (port)
		   (let ((lst (scanf-read-list "%[^:]%[:] %s" port)))
		     (and (pair? lst)
			  (eqv? 3 (length lst))
			  (apply string-append lst))))))))))
    ;;((NOSVE)				)
    ((UNIX COHERENT)
     (lambda ()
       (call-with-tmpnam
	(lambda (tmp)
	  (and (zero? (system (string-append "pwd >" tmp)))
	       (file-exists? tmp)
	       (let ((path (call-with-input-file tmp read-line)))
		 (and (string? path) path)))))))
    ;;((VMS)				)
    (else #f)))

;;@body
;;Creates a sub-directory @1 of the current-directory.  If successful,
;;@0 returns #t; otherwise #f.
(define (make-directory name)
  (zero? (system (string-append "mkdir " name))))

;;@body
;;Returns #t if changing directory to @1 makes the current working
;;directory the same as it is before changing directory; otherwise
;;returns #f.
(define (null-directory? file-name)
  (member file-name '("" "." "./" ".\\")))

;;@body
;;Returns #t if @1 is a fully specified pathname (does not depend on
;;the current working directory); otherwise returns #f.
(define (absolute-path? file-name)
  (and (string? file-name)
       (positive? (string-length file-name))
       (memv (string-ref file-name 0) '(#\\ #\/))))


;;@body Returns #t if the string @1 contains characters used for
;;specifying glob patterns, namely @samp{*}, @samp{?}, or @samp{[}.
(define (glob-pattern? str)
  (let loop ((idx (+ -1 (string-length str))))
    (if (negative? idx)
	#f
	(case (string-ref str idx)
	  ((#\* #\[ #\?) #t)
	  (else (loop (+ -1 idx)))))))

;;@body
;;Returns a list of the decoded FTP @1; or #f if indecipherable.  FTP
;;@dfn{Uniform Resource Locator}, @dfn{ange-ftp}, and @dfn{getit}
;;formats are handled.  The returned list has four elements which are
;;strings or #f:
;;
;;@enumerate 0
;;@item
;;username
;;@item
;;password
;;@item
;;remote-site
;;@item
;;remote-directory
;;@end enumerate
(define (parse-ftp-address uri)
  (define length? (lambda (len lst) (and (eqv? len (length lst)) lst)))
  (cond
   ((not uri) #f)
   ((length? 1 (scanf-read-list " ftp://%s %s" uri))
    => (lambda (host)
	 (let ((login #f) (path #f) (dross #f))
	   (sscanf (car host) "%[^/]/%[^@]%s" login path dross)
	   (and login
		(append (cond
			 ((length? 2 (scanf-read-list "%[^@]@%[^@]%s" login))
			  => (lambda (userpass@hostport)
			       (append
				(cond ((length? 2 (scanf-read-list
						   "%[^:]:%[^@/]%s"
						   (car userpass@hostport))))
				      (else (list (car userpass@hostport) #f)))
				(cdr userpass@hostport))))
			 (else (list "anonymous" #f login)))
			(list path))))))
   (else
    (let ((user@site #f) (colon #f) (path #f) (dross #f))
      (case (sscanf uri " %[^:]%[:]%[^@] %s" user@site colon path dross)
	((2 3)
	 (let ((user #f) (site #f))
	   (cond ((or (eqv? 2 (sscanf user@site "/%[^@/]@%[^@]%s"
				      user site dross))
		      (eqv? 2 (sscanf user@site "%[^@/]@%[^@]%s"
				      user site dross)))
		  (list user #f site path))
		 ((eqv? 1 (sscanf user@site "@%[^@]%s" site dross))
		  (list #f #f site path))
		 (else (list #f #f user@site path)))))
	(else
	 (let ((site (scanf-read-list " %[^@/] %s" uri)))
	   (and (length? 1 site) (list #f #f (car site) #f)))))))))

;;@body
;;@3 must be a non-empty string or #f.  @1 must be a non-empty list
;;of pathnames or Glob patterns (@pxref{Filenames}) matching files to
;;transfer.
;;
;;@0 puts the files specified by @1 into the @5 directory of FTP @4
;;using name @2 with (optional) @3.
;;
;;If @3 is #f and @2 is not @samp{ftp} or @samp{anonymous}, then @2 is
;;ignored; FTP takes the username and password from the @file{.netrc}
;;or equivalent file.
(define (ftp-upload paths user password remote-site remote-dir)
  (call-with-tmpnam
   (lambda (script logfile)
     (define local-path (current-directory))
     (define passwd (or password (user-email-address)))
     (dynamic-wind
      (lambda () #f)
      (lambda ()
	(call-with-current-continuation
	 (lambda (exit)
	   (define (run-ftp-script paths)
	     (call-with-output-file script
	       (lambda (port)
		 (define lcd "")
		 (cond ((or (member user '(ftp anonymous "ftp" "anonymous"))
			    password)
			(fprintf port "user %s %s\n" user passwd)))
		 (fprintf port "binary\n") ; Turn binary ON for all transfers
		 ;;(fprintf port "prompt\n") ; Turn prompt OFF for possible mget
		 (if (not (null-directory? remote-dir))
		     (fprintf port "cd %s\n" remote-dir))
		 (for-each
		  (lambda (path-name)
		    (let* ((r/i (string-reverse-index path-name #\/))
			   (dir (if r/i (substring path-name 0 (+ 1 r/i)) ""))
			   (file-name (if r/i
					  (substring path-name (+ 1 r/i)
						     (string-length path-name))
					  path-name)))
		      (cond ((and r/i (glob-pattern? dir))
			     (slib:warn
			      "Wildcard not allowed in directory component "
			      path-name)
			     (exit #f))
			    ((and (not (glob-pattern? file-name))
				  (not (file-exists? path-name)))
			     (slib:warn " file doesn't exist:" path-name)
			     (exit #f))
			    ((equal? lcd dir))
			    ((absolute-path? dir)
			     (fprintf port "lcd %s\n" dir))
			    ((eqv? 0 (substring? lcd dir))
			     (fprintf port "lcd %s\n"
				      (substring dir (string-length lcd)
						 (string-length dir))))
			    (else
			     (fprintf port "lcd %s\n" local-path)
			     (if (not (null-directory? dir))
				 (fprintf port "lcd %s\n" dir))))
		      (set! lcd dir)
		      (cond ((glob-pattern? file-name)
			     (fprintf port "mput %s\n" file-name))
			    (else
			     (fprintf port "put %s\n" file-name)))))
		  paths)))
	     ;;(display-file script)
	     (cond
	      ((zero? (system
		       (string-append
			"ftp "
			(if (or (member user '(ftp anonymous "ftp" "anonymous"))
				password)
			    "-inv" "-iv")
			" " remote-site
			" <" script
			" >" logfile)))
	       (file-exists? logfile)
	       (call-with-input-file logfile
		 (lambda (port)
		   (do ((line (read-line port) (read-line port)))
		       ((or (eof-object? line)
			    (substring-ci? "Unknown host" line)
			    (substring-ci? "Not connected" line)
			    (and (memv (string-ref line 0) '(#\4 #\5))
				 (not (substring-ci? "bytes" line))))
			(cond ((eof-object? line) #t)
			      (else (slib:warn line) #f)))
		     ;;(write-line line)
		     ))))
	      (else (slib:warn 'ftp 'failed) #f)))
	   (cond ((or local-path (every? absolute-file? paths))
		  (run-ftp-script paths))
		 (else (for-each (lambda (path) (run-ftp-script (list path)))
				 paths))))))
      (lambda ()
	(if (file-exists? script) (delete-file script))
	(if (file-exists? logfile) (delete-file logfile)))))
   2))

;;@body
;;Returns a URI-string for @1 on the local host.
(define (path->uri path)
  (if (absolute-path? path)
      (sprintf #f "file:%s" path)
      (sprintf #f "file:%s/%s" (current-directory) path)))

;;@body
;;If a @samp{netscape} browser is running, @0 causes the browser to
;;display the page specified by string @1 and returns #t.
;;
;;If the browser is not running, @0 runs @samp{netscape} with the
;;argument @1.  If the browser starts as a background job, @0 returns
;;#t immediately; if the browser starts as a foreground job, then @0
;;returns #t when the browser exits; otherwise it returns #f.
(define (browse-url-netscape url)
  (or (eqv? 0 (system (sprintf #f "netscape-remote -remote 'openURL(%s)'" url)))
      (eqv? 0 (system (sprintf #f "netscape -remote 'openURL(%s)'" url)))
      (eqv? 0 (system (sprintf #f "netscape '%s'&" url)))
      (eqv? 0 (system (sprintf #f "netscape '%s'" url)))))
