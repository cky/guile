#!/usr/bin/awk -f
BEGIN { FS="|"; 
        filename = ARGV[1]; ARGV[1] = ""; 
        dot_x_file = filename; dot_doc_file = filename;
	sub(/\..*$/,".x",dot_x_file);
	sub(/\..*$/,".doc",dot_doc_file); 
}

/^[ \t]*%%%/ { copy = $0; gsub(/[ \t]*%%%/, "", copy); gsub(/\$\$\$.*$/, "", copy); print copy > dot_x_file  }

/\$\$\$/,/@@@/ { copy = $0; 
                 gsub(/.*\$\$\$./,"", copy); 
                 gsub(/@@@.*/,"",copy); 
                 gsub(/[ \t]+/," ", copy); 
		 sub(/^[ \t]*/,"(", copy);
#		 gsub(/.\".*\"/, toupper("&"), copy);
                 gsub(/\"/,"",copy); 
		 sub(/ \(/," ",copy);
		 numargs = gsub(/SCM /,"", copy);
		 numcommas = gsub(/,/,"", copy);
		 numactuals = $2 + $3 + $4;
		 location = $5;
		 gsub(/\"/,"",location);
		 gsub(/^[ \t]*/,"",location);
		 gsub(/[ \t]*\|.*$/,"",copy);
		 if (numargs != numactuals) { print location ":*** `" copy "' is improperly registered as having " numactuals " arguments"; }
		 print "" copy > dot_doc_file ; }

/@@@/,/@!!![ \t]*$/ { copy = $0; 
                      gsub(/.*@@@/,"",copy); 
		      gsub(/^"/,"", copy);
		      gsub(/\"[ \t]*@!!![ \t]*$/,"", copy);
                      gsub(/\\\"/,"\"",copy);
                      print copy > dot_doc_file ; }
/@!!![ \t]*$/ { print "[" location "]\n" >> dot_doc_file; }

