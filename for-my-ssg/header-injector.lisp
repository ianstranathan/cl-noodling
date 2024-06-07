
(defparameter website-root-name "https://ianstranathan.github.io/")
(defparameter root-path-name "c:/_work/website/pages/")


(defun index-p ( pathspec )
  (probe-file pathspec))


(defun all-dirs (dir-path)
  ;; merge-pathname will truncate without trailing backslash
  (merge-pathnames "*" dir-path))


(defun pathname-last-dir (pathspec)
  (let ((name (first (last (pathname-directory pathspec)))))
    (if (string= name "pages")
	"Main"
	(string-capitalize name))))


(defun make-pathname-into-link (a-namestring)
  (if (string= a-namestring root-path-name)
      website-root-name
      (concatenate 'string website-root-name
		   (subseq a-namestring (length root-path-name)))))


(defun interleave-links-and-link-names (a-pathname) ; -> ls
  (labels ((acc-link ( acc another-pathname)
	     ;; interleave name and link
	     (let ((link-name (pathname-last-dir another-pathname))
		   (link (make-pathname-into-link (namestring another-pathname))))
	       (cons link (cons link-name acc))))
	   (recur (acc curr-pathname)
	     (if (string= (namestring curr-pathname) root-path-name)
		 acc
		 (let ((parent-dir (uiop:pathname-parent-directory-pathname curr-pathname)))
		   (recur (acc-link acc parent-dir) parent-dir)))))
    (recur (acc-link nil a-pathname) a-pathname)))


(defun link-injection (pathspec)
  (if (index-p pathspec) ; if there's an index.html
      (let* ((file-str (uiop:read-file-string pathspec)) ; html file as a string
	     (end   (search "</header>" file-str))       ; [0 to </header>]
	     (start-str (subseq file-str
				0
				(+ (length "<header>")
				   (search "<header>" file-str))))
	     (end-str   (subseq file-str end)))
	(with-open-file (stream pathspec
			   :direction :output
			   :if-exists :supersede)
          (format stream "~{~a~a~a~}" `(,start-str
				        ,(format nil
						"~{ <a href=\"~a\">~a</a> ::~}"
						(interleave-links-and-link-names
						 (uiop:pathname-parent-directory-pathname pathspec)))
					,end-str))))))


(defun walk-pages-dir ()
  (labels ((recur (ls-dirs)
	     (mapcar (lambda (dir-pathname)
		       (let (( subdirs (directory (all-dirs dir-pathname))))
			 (if subdirs
			     ;; if there is a directory keep recursing
			     (progn (print subdirs)
				    (recur subdirs))
			     ;; otherwise do stuff to the html file	     
			     (link-injection (merge-pathnames "index.html" dir-pathname)))))
		     ls-dirs)))
    (recur (directory (all-dirs root-path-name)))))
