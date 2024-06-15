
;; (defparameter site-root-url "https://ianstranathan.github.io/")
(defparameter site-root-url "http://localhost:8080/")
(defparameter root-pathname #P"c:/_work/website/html/")

;; ---------------------------------------------------------------------------
;; Utils
;; -----
(defun index-p (pathspec)
  (if pathspec
      (probe-file (merge-pathnames "index.html" pathspec))))

(defun all-dirs-ls (pathspec) ; -> ls
  (directory (merge-pathnames "*" pathspec)))

(defun pathname-last-dir (pathspec)
  (first (last (pathname-directory pathspec))))

(defun pathname-assert (path &optional (another-path #P"c:/") (fn #'>))
  ;; (uiop:pathname-parent-directory-pathname #P"c:/") loops forever
  (assert (apply fn (mapcar (lambda (x) (length (namestring x))) (list path another-path)))))

(defun make-link-url-from-pathspec (pathspec)
  (if (eql pathspec root-pathname)
      site-root-url
      (format nil "~a~a"
	      site-root-url
	      (subseq (namestring pathspec) ; offset into pathname by root url
		      (length (namestring root-pathname))))))

(defun remove-file-from-path-pathname (file-pathspec)
  (make-pathname :directory (pathname-directory file-pathspec)
		 :name nil :type nil))

;; ---------------------------------------------------------------------------

(defun interleave-links-and-link-names-for-header (pathspec) ; -> ls
  "takes in a pathname and returns a list of interleaved names and links of its parent dirs"
  (labels ((header-link-name (a-pathname)
	     (let ((name (namestring (pathname-last-dir a-pathname))))    
	       (if (string= name "html")
		   "Main"
		   (string-capitalize name))))
	   (acc-link (acc a-pathname)
	     ;; interleave name and link
	     (let ((link-name (header-link-name a-pathname))
		   (link      (make-link-url-from-pathspec a-pathname)))
	       (cons link (cons link-name acc))))
	   ;; --------------------
	   (recur (acc a-pathname)
	     (pathname-assert a-pathname)
	     (if (eql a-pathname root-pathname)
		 acc
		 (let ((parent-dir (uiop:pathname-parent-directory-pathname a-pathname)))
		   (recur (acc-link acc parent-dir) parent-dir)))))
    (recur (acc-link nil pathspec) pathspec)))


(defun html-injection (file &optional (pathspec nil))
  
  ;; ;; stack variables are just a way I'm cutting up an html file as a string
  ;; ;; it's probably better to be building up component wise, rather than cutting down
  ;; ;; but I'm using org mode and it's enough for what I want
  (let* ((file-str   (uiop:read-file-string file))
	 (header     (format nil "<header>~{ <a href=\"~a\">~a</a> ::~}</header>"
			     (interleave-links-and-link-names-for-header
			      (uiop:pathname-parent-directory-pathname file))))
    
	 (start-2-header      (subseq file-str 0 (search "<header>" file-str)))
	 (end-of-header-2-end (subseq file-str (search "</header>" file-str :from-end t)))
	 
	 ;; superposition logic here to avoid loading file more than once on a page with links
	 (content-end-index (if pathspec
				;; need to offset to not include </div> at beg of main seq
				(search "</div>" end-of-header-2-end :start2 1)))
	 (rest-with-content (if content-end-index
				(format nil "~{~a~}" (list (subseq end-of-header-2-end 0 content-end-index)
							   (page-content-str pathspec)
							   (subseq end-of-header-2-end content-end-index )))
				end-of-header-2-end)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede)
      (format stream "~{~a~}" (list start-2-header header rest-with-content)))))


(defun finish-website()
  ;; runs through all directories in root directory
  ;; injects header and page links if it's a page (index.html exists && subdirs exist)
  (let ((pathnames ()))
    (labels ((walk (dir)
	       (cond
		 ((null dir))
		 ((atom dir)
		  (push dir pathnames) ;; debug collection
		  (let ((subdirs (all-dirs-ls dir))
			(index-file (index-p dir)))
		    (if (eql dir root-pathname)
			(walk subdirs))
		    (if index-file
			(if subdirs
			    (html-injection index-file dir) ;; page injection & header
			    (html-injection index-file)))     ;; header
		    (walk subdirs)))
		 (t (walk (car dir))
		    (walk (cdr dir))))))
      (walk root-pathname))
    (nreverse pathnames)))

(defun remove-file-from-path-pathname (file-pathspec)
  (make-pathname :directory (pathname-directory file-pathspec)
		 :name nil :type nil))

(defun page-content-str (pathspec)
  (let ((html-str ()))
    (labels ((template-str (path)
	       (if (index-p path)
		   (format nil "<a href=\"~a\">~a</a>~C"
			   (make-link-url-from-pathspec path)
			   (pathname-last-dir path)
			   #\Newline)
		   (format nil "<p>~a</p>~C"
			   (pathname-last-dir path)
			   #\Newline)))
	     (walk (dir)
	       (cond
		 ((null dir))
		 ((atom dir)
		  (push (template-str dir) html-str)
		  (walk (all-dirs-ls dir)))
		 (t (walk (car dir))
		    (walk (cdr dir))))))
      (walk pathspec))
    (apply #'concatenate 'string (nreverse html-str))))

