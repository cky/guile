
(defun html-page (title &rest contents)
  (concat "<HTML>\n"
	  "<HEAD>\n"
	  "<TITLE>" title "</TITLE>\n"
	  "</HEAD>\n"
	  "<BODY>\n"
	  (apply 'concat contents)
	  "</BODY>\n"
	  "</HTML>\n"))
