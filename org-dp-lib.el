;;; org-dp-lib.el --- Library of org-dp functions
;; Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
;; Version: 0.9
;; URL: https://github.com/tj64/org-dp

;;;; MetaData
;;   :PROPERTIES:
;;   :copyright: Thorsten Jolitz
;;   :copyright-years: 2014+
;;   :version:  0.9
;;   :licence:  GPL 3 or later (free software)
;;   :licence-url: http://www.gnu.org/licenses/
;;   :part-of-emacs: no
;;   :author: Thorsten Jolitz
;;   :author_email: tjolitz AT gmail DOT com
;;   :keywords: emacs org-mode org-elements declarative-programming
;;   :git-repo: https://github.com/tj64/org-dp
;;   :git-clone: git://github.com/tj64/org-dp.git
;;   :END:

;;;; Commentary
;; Library of functions written with org-dp.el.

;;; Requires

(require 'org-dp)

;;; Variables
;;;; Vars
;;;; Consts

(defconst org-dp-interpreted-keys-alist
  (list
   '(center-block . ((contents . "%s ipsum")))
   '(drawer . ((:drawer-name . (make-temp-name "drawer"))
	       (contents . "%s ipsum")))
   '(dynamic-block . ((:block-name . (make-temp-name "dynblock"))
		      (:arguments . nil)
		      (contents . "%s ipsum")))
   '(footnote-definition . ((:label . (make-temp-name "fn-def"))
			    (contents . "%s ipsum")))
   '(headline . ((:level . (1 2 3 4 5 6 7 8))
		 (:todo-keyword
		  . ("TODO" "NEXT" "WAITING" "DONE" nil nil nil))
		 (:priority
		  . (?A ?B ?C nil nil nil nil nil nil nil nil nil))
		 (:title . (format
			    "headline %s"
			    (let* ((stamp
				    (cdr
				     (assoc
				      'timestamp
				      org-dp-lorem-objects-2)))
				   (cookie
				    (cdr (assoc
					  'statistics-cookie
					  org-dp-lorem-objects-2)))
				   (lst
				    (list stamp stamp stamp stamp
					  stamp stamp cookie ""
					  "" "" "" "" "" "")))
			      (nth (random (length lst)) lst))))
		 (:archivedp
		  . (t nil nil nil nil nil nil nil nil nil nil nil))
		 (:tags
		  . (("home" "office") ("home") ("office") nil nil))
		 (:commentedp
		  . (t nil nil nil nil nil nil nil nil nil nil nil))
		 (:pre-blank . (1 1 1 1 1 1 1 1 1 1 1 1 2 nil))
		 (:footnote-section-p
		  . (t nil nil nil nil nil nil nil nil nil nil nil))
		 (contents . "%s ipsum")))
   '(inline-task . ((:level . (1 2 3 4 5 6 7 8))
		    (:todo-keyword
		     . ("TODO" "NEXT" "WAITING" "DONE" nil nil nil))
		    (:priority
		     . (?A ?B ?C nil nil nil nil nil nil nil nil nil))
		    (:title . (format
			       "headline %s"
			       (let* ((stamp
				       (cdr
					(assoc
					 'timestamp
					 org-dp-lorem-objects-2)))
				      (cookie
				       (cdr (assoc
					     'statistics-cookie
					     org-dp-lorem-objects-2)))
				      (lst
				       (list stamp stamp stamp stamp
					     stamp stamp cookie ""
					     "" "" "" "" "" "")))
				 (nth (random (length lst)) lst))))
		    (:tags
		     . (("home" "office") ("home") ("office")
			nil nil))
		    (contents . "%s ipsum")))
   '(item . ((:bullet . ("-" "+"))
	     (:checkbox . "[ ]")
	     (:counter . nil)
	     (:tag . (make-temp-name "item"))
	     (contents . "%s ipsum")))
   '(plain-list . ((contents . (concat
				" - lorem :: ipsum\n"
				" - dolor :: sit\n"))))
   '(property-drawer . ((contents .(concat
				    " :lorem: ipsum\n"
				    " :dolo: sit\n"))))
   '(quote-block . ((contents . "%s ipsum")))
   '(section . ((contents . "%s ipsum")))
   '(special-block . ((:type . ("LATEX" "HTML" "ORG"))
		      (contents . "%s ipsum")))
   '(babel-call . ((:value . "%s")))
   '(clock . ((:value
	       . "[2014-08-15 Fr 17:17]--[2014-08-15 Fr 17:20]")
	      (:duration . "0:03")))
   '(comment . ((:value . "%s ipsum")))
   '(comment-block . ((:value . "%s ipsum")))
   '(diary-sexp . ((:value
		    ."%%(org-anniversary 1956 5 14) A.D %dy old" )))
   '(example-block . ((:switches . ("-n -r" "-n" "-r" "" "" "" ""))
		      (:preserve-intent . (1 nil)) ; FIXME number?
		      (:value . "%s ipsum")))
   '(fixed-width . ((:value . "%s ipsum")))
   '(horizontal-rule . (("-----")))
   '(keyword . ((:key . "TODO")
		(:value . "TODO | DONE")))
   ;; '(keyword . ((:key . (make-temp-name "kw"))
   ;; 	      (:value . (make-temp-name "val"))))
   '(latex-environment . ((:value . " a=+\sqrt{2} ")))
   '(node-property . ((:key . (make-temp-name "prop"))
		      (:value . (make-temp-name "val"))))
   '(paragraph . ((contents . "%s ipsum")))
   '(planning . ((:deadline . "<2014-08-15 Fr 22:00>")
		 (:scheduled
		  . "<2014-08-15 Fr 20:00>--<2014-08-15 Fr 21:00>")
		 (:closed . nil)))
   '(scr-block . ((:language . (mapcar
				(lambda (--lang)
				  (symbol-name (car --lang)))
				org-babel-load-languages))
		  (:switches . ("-n -r" "-n" "-r" "" "" "" "" ""))
		  (:parameters . ":var x=5 :results raw")
		  (:value . "x")
		  (:preserve-indent . (1 nil)))) ; FIXME number?
   '(table . ((:type . (org table.el))
	      (:value . "| %s | ipsum |\n| lorem | ipsum |\n")
	      (:tblfm . "$5=taylor($2,%4,%3);n3")))
   '(table-row . ((:type . (rule standard))
		  (contents . "%s ipsum")))
   '(quote-block . ((contents . "%s ipsum"))))
  "AList of elements and their interpreted keywords, with
  example values.")

(defconst org-dp-lorem-objects-1
  (list
   '(bold . "*lorem*")
   '(code . "~lorem~")
   '(entity . "\\alpha")
   '(italic . "/lorem/")
   '(strike-through . "+lorem+")
   '(subscript . "_{lorem}")
   '(superscript . "^{lorem}")
   '(underline . "_lorem_")
   '(verbatim . "=lorem="))
  "AList 1 of object types and examples.")

(defconst org-dp-lorem-objects-2
  (list
   '(export-snippet . "@@ascii:lorem@@")
   '(footnote-reference . "[fn::lorem]")
   '(inline-babel-call . "call_lorem()")
   '(inline-src-block . "src_emacs-lisp[:var x=lorem]{x}")
   '(latex-fragment
     . "\begin{equation}\nlorem=\sqrt{x}\n\end{equation}")
   '(line-break . "lorem\\\\\n")
   '(link-interpreter . "[[http://orgmode.org/][Lorem]]")
   '(macro . "{{{lorem(arg1, arg2)}}}")
   '(radio-target . "<<<lorem>>>")
   '(statistics-cookie . "[33%]")
   '(table-cell . " lorem |")
   '(target . "<<lorem>>")
   '(timestamp . "<2014-08-14 Do 20:53>"))
  "AList 2 of object types and examples.")

;;; Functions
;;;; Random Org Elements

(defun org-dp--calc-val (val)
  "Select or calc example value from VAL."
  (let ((objs org-dp-lorem-objects-1))
    (cond
     ((and (stringp val)
	   (string-match "%s" val))
      (format val (cdr (nth (random (length objs)) objs))))
     ((and (consp val)
	   (functionp (car val))
	   (eval val)))
     ((consp val) (nth (random (length val)) val))
     ((and (not (booleanp val))
	   (symbolp val)
	   (let ((symval (symbol-value val)))
	     (if (consp symval)
		 (nth (random (length symval)) symval)
	       symval))))
     (t val))))

(defun org-dp--calc-args (lst &optional level)
  "Select or calc args from LST.
Optional arg LEVEL allows to set pre-calculated headline-levels."
  (let ((return-lst)
	(arg-lst (delq (assoc 'contents lst) lst)))
    (while arg-lst
      (let ((--arg (pop arg-lst)))
	(setq return-lst
	      (append (list (car --arg))
		      (list (or
			     (and (eq (car --arg) :level) level)
			     (org-dp--calc-val (cdr --arg))))
		      return-lst))))
    (message "Return list: %s" return-lst)
    return-lst))
		
;;; Commands
;;;; Wrap in Block

(defun org-dp-wrap-in-block (&optional type lines &rest headers)
  "Wrap sexp-at-point or region in Org block.

Use block TYPE if given, an Emacs-Lisp src-block otherwise. A
region instead of the sexp-at-point is wrapped if either

   - optional arg LINES is an (positive or negative) integer or
   - the region is active

In the first case the region is determined by moving +/- LINES
forward/backward from point using `forward-line', in the second
case the active region is used.

If point is already inside of a block, modify or delete this
block instead of wrapping it in another block.

When called with prefix argument 'C-u', prompt the user for the
Org-Babel language to use. When called with two prefix arguments
'C-u C-u', prompt the user for both the Org-Babel language to use
and the number of lines to be wrapped.

If `current-prefix-arg' is non-nil and block-type is `src',
prompt the user for additional header-args."
  (interactive
   (let* (;; point in block 
	  (in-block
	   (let* ((dblock-limits (org-between-regexps-p
				  org-dblock-start-re
				  org-dblock-end-re))
		  (src-block-beg
		   (unless dblock-limits
		     (org-babel-where-is-src-block-head)))
		  (block-limits
		   (unless (or dblock-limits src-block-beg)
		     (org-in-regexp org-block-regexp 1000))))
	     (when
		 (or block-limits dblock-limits src-block-beg)
	       (cond
		;; remove surrounding block
		((y-or-n-p "Unwrap block content ")
		 (cond
		  (dblock-limits
		   (save-excursion
		     (goto-char (car dblock-limits))
		     (org-dp-rewire 'paragraph t t)))
		  (src-block-beg
		   (save-excursion
		     (goto-char src-block-beg)
		     (org-dp-rewire
		      'paragraph
		      (lambda (_cont_ elem)
			(org-element-property :value elem)) t)))
		  (block-limits
		   (save-excursion
		     (goto-char (car block-limits))
		     (org-dp-rewire
		      'paragraph
		      (lambda (_cont_ elem)
			(let ((type (org-element-type elem)))
			  (case type
			    ((center-block
			      quote-block special-block)
			     (org-dp-contents elem t))
			    ((comment-block example-block)
			     (org-element-property :value elem))
			    (t nil))))
		      t))))
		 (user-error "Unwrapped block content."))
		;; replace surrounding block
		((y-or-n-p "Replace surrounding block ")
		 (save-excursion
		   (goto-char (or (car dblock-limits)
				  src-block-beg
				  (car block-limits)))
		   (let ((user-info
			  (org-dp-prompt))))
		     (org-dp-rewire parsed-block t nil nil
					;; TODO prompt user!
					:type 'paragraph)
		   (user-error "Replaced surrounding block.")))
		 ;; ;; src-blocks
		 ;; (if parsed-src-block
		 ;;     (progn
		 ;;       (save-excursion
		 ;; 	 (goto-char
		 ;; 	  (1- (org-element-property
		 ;; 	       :end parsed-src-block)))
		 ;; 	 (kill-whole-line)
		 ;; 	 (push-mark))
		 ;;       (delete-region
		 ;; 	(org-element-property
		 ;; 	 :begin parsed-src-block)
		 ;; 	(org-element-property
		 ;; 	 :post-affiliated parsed-src-block)))
		 ;;   ;; other blocks
		 ;;   (save-excursion
		 ;;     (goto-char (or (cdr-safe block-limits)
		 ;; 		    (cdr-safe dblock-limits)))
		 ;;     (save-excursion
		 ;;       (forward-line -1) (push-mark))
		 ;;     (kill-whole-line))
		 ;;   (goto-char (or (car-safe block-limits)
		 ;; 		  (car-safe dblock-limits)))
		 ;;   (kill-whole-line)))
		;; nest blocks or abort
		(t (if (y-or-n-p "Really nest blocks ")
		       nil
		     (user-error "Action aborted.")))))))
	  ;; point not in block
	  (babel-lang-types (mapcar
			     (lambda (--lang)
			       (symbol-name (car --lang)))
			     org-babel-load-languages))
	  (org-block-types (delete "src"
				   (mapcar
				    (lambda (--block)
				      (downcase (car --block)))
				    org-element-block-name-alist)))
	  (all-types (append babel-lang-types org-block-types
			     (list "dynamic" "special")))
	  (ido-read-block-type '(ido-completing-read
				 "Block Type: " all-types
				 nil nil nil nil "emacs-lisp"))
	  ;; (ido-read-headers '(while (y-or-n-p "Add header ")
	  ;; 		       (cons
	  ;; 			(ido-completing-read
	  ;; 			 "arg: "
	  ;; 			 (mapcar
	  ;; 			  'symbol-name
	  ;; 			  org-babel-header-arg-names)
	  ;; 			 nil nil nil nil "var")
	  ;; 			(read-string "value: "))))
	  (read-dyn-params '(while (y-or-n-p "Add parameter ")
			      (cons
			       (read-string "param: ")
			       (read-string "value: ")))))
     (cond
      ((equal current-prefix-arg nil) nil)
      ((equal current-prefix-arg '(4))
       (let ((typ (eval ido-read-block-type)))
	 (list typ nil	       
	       (cond
		((member-ignore-case typ babel-lang-types)
		 (while (y-or-n-p "Add header ")
		   (tj/prompt-for-header typ)))
		;; (eval ido-read-headers))
		((string= typ "dynamic")
		 (eval read-dyn-params))
		((string= typ "special")
		 (read-string "Name: "))))))
      ((equal current-prefix-arg '(16))
       (let ((typ (eval ido-read-block-type)))
	 (list typ
	       (read-number "Number of lines to wrap: " 1)
	       (cond
		((member-ignore-case typ babel-lang-types)
		 (while (y-or-n-p "Add header ")
		   (tj/prompt-for-header typ)))
		;; (eval ido-read-headers))
		((string= typ "dynamic")
		 (eval read-dyn-params))
		((string= typ "special")
		 (read-string "Name: "))))))
      (current-prefix-arg
       (list nil nil ido-read-headers)))))
  (if (called-interactively-p 'any) 
      (let* ((block-type (or type "emacs-lisp"))
	     (beg (or (and (not lines)
			   (region-active-p)
			   (region-beginning))
		      (point)))
	     (marker (save-excursion (goto-char beg) (point-marker)))
	     (bol (save-excursion (goto-char beg) (bolp)))
	     (end (cond
		   (lines (save-excursion
			    (forward-line lines) (point)))
		   ((region-active-p)(region-end))
		   (t (save-excursion
			(forward-sexp) (point)))))
	     (cut-strg (buffer-substring beg end))
	     (babel-langs (mapcar
			   (lambda (--lang)
			     (symbol-name (car --lang)))
			   org-babel-load-languages))
	     (block-names (delete "src"
				  (mapcar
				   (lambda (--block)
				     (downcase (car --block)))
				   org-element-block-name-alist))))
	(delete-region beg end)
	(goto-char (marker-position marker))
	(insert
	 (format
	  "%s#+begin%s\n%s%s#+end%s\n"
	  (if (or (and lines (< lines 0)) bol) "" "\n")
	  (cond
	   ((member block-type babel-langs)
	    (format "_src %s" block-type))
	   ((member block-type block-names)
	    (concat "_" block-type))
	   (t ": "))
	  cut-strg
	  (if lines "" "\n")
	  (cond
	   ((member block-type babel-langs) "_src")
	   ((member block-type block-names) (concat "_" block-type))
	   (t ": "))))
	(save-excursion
	  (forward-line -1)
	  (while headers
	    (let ((pair (pop headers)))
	      (tj/insert-header-arg
	       (car pair) (cdr pair) 'KEYWORD-P))))
	(set-marker marker nil))
    (call-interactively 'tj/wrap-in-block)))

;; (defun org-dp-wrap-in-block (&optional type lines &rest headers)
;;   "Wrap sexp-at-point or region in Org block.

;; Use block TYPE if given, an Emacs-Lisp src-block otherwise. A
;; region instead of the sexp-at-point is wrapped if either

;;    - optional arg LINES is an (positive or negative) integer or
;;    - the region is active

;; In the first case the region is determined by moving +/- LINES
;; forward/backward from point using `forward-line', in the second
;; case the active region is used.

;; If point is already inside of a block, modify or delete this
;; block instead of wrapping it in another block.

;; When called with prefix argument 'C-u', prompt the user for the
;; Org-Babel language to use. When called with two prefix arguments
;; 'C-u C-u', prompt the user for both the Org-Babel language to use
;; and the number of lines to be wrapped.

;; If `current-prefix-arg' is non-nil and block-type is `src',
;; prompt the user for additional header-args."
;;   (interactive
;;    (let* (;; point in block 
;; 	  (in-block
;; 	   (let* ((dblock-limits (org-between-regexps-p
;; 				  org-dblock-start-re
;; 				  org-dblock-end-re))
;; 		  (src-block-beg
;; 		   (unless dblock-limits
;; 		     (org-babel-where-is-src-block-head)))
;; 		  (block-limits
;; 		   (unless (or dblock-limits src-block-beg)
;; 		     (org-in-regexp org-block-regexp 1000))))
;; 	     (when
;; 		 (or block-limits dblock-limits src-block-beg)
;; 	       (cond
;; 		;; remove surrounding block
;; 		((y-or-n-p "Unwrap block content ")
;; 		 (save-excursion
;; 		   (goto-char (or (car dblock-limits)
;; 				  src-block-beg
;; 				  (car block-limits)))
;; 		   (let* ((parsed-block (org-element-at-point))
;; 			  (type (org-element-type parsed-block))
;; 			  (val-or-cont
;; 			   (if (or
;; 				(eq type 'verse-block)
;; 				(memq type
;; 				      org-element-greater-elements))
;; 			       (org-dp-contents parsed-block t)
;; 			     (org-element-property :value
;; 						   parsed-block))))
;; 		     (insert
;; 		      (org-element-paragraph-interpreter
;; 		       (org-dp-rewire parsed-block t nil nil
;; 					  :type 'paragraph)
;; 		       val-or-cont)))
;; 		   (user-error "Unwrapped block content.")))
;; 		;; replace surrounding block
;; 		((y-or-n-p "Replace surrounding block ")
;; 		 (save-excursion
;; 		   (goto-char (or (car dblock-limits)
;; 				  src-block-beg
;; 				  (car block-limits)))
;; 		   (let* ((parsed-block
;; 			   (org-element-at-point))
;; 			  (type (org-element-type parsed-block)))
;; 		     (org-dp-rewire parsed-block t nil nil
;; 					;; TODO prompt user!
;; 					:type 'paragraph))
;; 		   (user-error "Replaced surrounding block.")))
;; 		 ;; ;; src-blocks
;; 		 ;; (if parsed-src-block
;; 		 ;;     (progn
;; 		 ;;       (save-excursion
;; 		 ;; 	 (goto-char
;; 		 ;; 	  (1- (org-element-property
;; 		 ;; 	       :end parsed-src-block)))
;; 		 ;; 	 (kill-whole-line)
;; 		 ;; 	 (push-mark))
;; 		 ;;       (delete-region
;; 		 ;; 	(org-element-property
;; 		 ;; 	 :begin parsed-src-block)
;; 		 ;; 	(org-element-property
;; 		 ;; 	 :post-affiliated parsed-src-block)))
;; 		 ;;   ;; other blocks
;; 		 ;;   (save-excursion
;; 		 ;;     (goto-char (or (cdr-safe block-limits)
;; 		 ;; 		    (cdr-safe dblock-limits)))
;; 		 ;;     (save-excursion
;; 		 ;;       (forward-line -1) (push-mark))
;; 		 ;;     (kill-whole-line))
;; 		 ;;   (goto-char (or (car-safe block-limits)
;; 		 ;; 		  (car-safe dblock-limits)))
;; 		 ;;   (kill-whole-line)))
;; 		;; nest blocks or abort
;; 		(t (if (y-or-n-p "Really nest blocks ")
;; 		       nil
;; 		     (user-error "Action aborted.")))))))
;; 	  ;; point not in block
;; 	  (babel-lang-types (mapcar
;; 			     (lambda (--lang)
;; 			       (symbol-name (car --lang)))
;; 			     org-babel-load-languages))
;; 	  (org-block-types (delete "src"
;; 				   (mapcar
;; 				    (lambda (--block)
;; 				      (downcase (car --block)))
;; 				    org-element-block-name-alist)))
;; 	  (all-types (append babel-lang-types org-block-types
;; 			     (list "dynamic" "special")))
;; 	  (ido-read-block-type '(ido-completing-read
;; 				 "Block Type: " all-types
;; 				 nil nil nil nil "emacs-lisp"))
;; 	  ;; (ido-read-headers '(while (y-or-n-p "Add header ")
;; 	  ;; 		       (cons
;; 	  ;; 			(ido-completing-read
;; 	  ;; 			 "arg: "
;; 	  ;; 			 (mapcar
;; 	  ;; 			  'symbol-name
;; 	  ;; 			  org-babel-header-arg-names)
;; 	  ;; 			 nil nil nil nil "var")
;; 	  ;; 			(read-string "value: "))))
;; 	  (read-dyn-params '(while (y-or-n-p "Add parameter ")
;; 			      (cons
;; 			       (read-string "param: ")
;; 			       (read-string "value: ")))))
;;      (cond
;;       ((equal current-prefix-arg nil) nil)
;;       ((equal current-prefix-arg '(4))
;;        (let ((typ (eval ido-read-block-type)))
;; 	 (list typ nil	       
;; 	       (cond
;; 		((member-ignore-case typ babel-lang-types)
;; 		 (while (y-or-n-p "Add header ")
;; 		   (tj/prompt-for-header typ)))
;; 		;; (eval ido-read-headers))
;; 		((string= typ "dynamic")
;; 		 (eval read-dyn-params))
;; 		((string= typ "special")
;; 		 (read-string "Name: "))))))
;;       ((equal current-prefix-arg '(16))
;;        (let ((typ (eval ido-read-block-type)))
;; 	 (list typ
;; 	       (read-number "Number of lines to wrap: " 1)
;; 	       (cond
;; 		((member-ignore-case typ babel-lang-types)
;; 		 (while (y-or-n-p "Add header ")
;; 		   (tj/prompt-for-header typ)))
;; 		;; (eval ido-read-headers))
;; 		((string= typ "dynamic")
;; 		 (eval read-dyn-params))
;; 		((string= typ "special")
;; 		 (read-string "Name: "))))))
;;       (current-prefix-arg
;;        (list nil nil ido-read-headers)))))
;;   (if (called-interactively-p 'any) 
;;       (let* ((block-type (or type "emacs-lisp"))
;; 	     (beg (or (and (not lines)
;; 			   (region-active-p)
;; 			   (region-beginning))
;; 		      (point)))
;; 	     (marker (save-excursion (goto-char beg) (point-marker)))
;; 	     (bol (save-excursion (goto-char beg) (bolp)))
;; 	     (end (cond
;; 		   (lines (save-excursion
;; 			    (forward-line lines) (point)))
;; 		   ((region-active-p)(region-end))
;; 		   (t (save-excursion
;; 			(forward-sexp) (point)))))
;; 	     (cut-strg (buffer-substring beg end))
;; 	     (babel-langs (mapcar
;; 			   (lambda (--lang)
;; 			     (symbol-name (car --lang)))
;; 			   org-babel-load-languages))
;; 	     (block-names (delete "src"
;; 				  (mapcar
;; 				   (lambda (--block)
;; 				     (downcase (car --block)))
;; 				   org-element-block-name-alist))))
;; 	(delete-region beg end)
;; 	(goto-char (marker-position marker))
;; 	(insert
;; 	 (format
;; 	  "%s#+begin%s\n%s%s#+end%s\n"
;; 	  (if (or (and lines (< lines 0)) bol) "" "\n")
;; 	  (cond
;; 	   ((member block-type babel-langs)
;; 	    (format "_src %s" block-type))
;; 	   ((member block-type block-names)
;; 	    (concat "_" block-type))
;; 	   (t ": "))
;; 	  cut-strg
;; 	  (if lines "" "\n")
;; 	  (cond
;; 	   ((member block-type babel-langs) "_src")
;; 	   ((member block-type block-names) (concat "_" block-type))
;; 	   (t ": "))))
;; 	(save-excursion
;; 	  (forward-line -1)
;; 	  (while headers
;; 	    (let ((pair (pop headers)))
;; 	      (tj/insert-header-arg
;; 	       (car pair) (cdr pair) 'KEYWORD-P))))
;; 	(set-marker marker nil))
;;     (call-interactively 'tj/wrap-in-block)))

;;;; Create Random Org Buffer

;; FIXME not yet working!
(defun org-dp-lorem-ipsum (&optional number-of-entries)
  "Create random Org document with N entries.
Use NUMBER-OF-ENTRIES if given, otherwise set N to 10."
  (interactive
   (when current-prefix-arg
     (list (read-number "Number of entries: " 100))))
  (let ((N (or number-of-entries 10))
	(elems org-dp-interpreted-keys-alist)
	last-level level rnd strg)
    (with-current-buffer (get-buffer-create "*Org Lorem Ipsum*")
      (dotimes (i N strg)
	;; get random number between 1 and 100
	(setq rnd (1+ (random 100)))
	;; calc headline level based on last level
	(setq level (case last-level
		      (1 (if (< rnd 51) 1 2))
		      ((number-sequence 2 7)
		       (cond
			((< rnd 26) (1- last-level))
			((< rnd 51) last-level)
			((< rnd 76) (1+ last-level))
			(t 1)))
		      (8 (cond
			  ((< rnd 34) 7)
			  ((< rnd 68) 8)
			  (t 1)))
		      (t 1)))
	;; create new headline
	(org-dp-create
	 ;; default type 'headline
	 nil
	 ;; maybe create contents
	 (when (< rnd 101)
	   (let ((cont (nth (random (length elems)) elems)))
	     ;; fix "contained only" elements
	     (case (car cont)
	       (item (setq cont (assoc 'plain-list elems)))
	       (horizontal-line
		(setq cont (assoc 'headline elems)))
	       (node-property (setq cont
				    (assoc 'property-drawer elems)))
	       (table-row (setq cont (assoc 'table elems))))
	     (if (eq (car cont) 'section)
		 ;; section only
		 (org-dp--calc-val (cdr cont))
	       ;; create section with element
	       (org-dp-create
		'section
		;; create element
		(apply
		 'org-dp-create
		 (car cont)
		 (org-dp--calc-val
		  (cdr-safe (assoc 'contents (cdr cont))))
		 nil
		 (org-dp--calc-args (cdr cont)))))))
	 ;; insert headline
	 'INSERT-P
	 ;; set headline's property list
	 (org-dp--calc-args
	  (cdr (assoc 'headline elems)) level))))))

;;;; Toggle Src-Block Headers

(defun org-dp-toggle-headers (&optional action)
  "Toggle header argument representation.
With prefix-arg, prompt user for ACTION, otherwise, if non-nil,
it should take one the values `swap', `header' or `param',
meaning to either swap :header and :parameter values, or make
them all :header or :parameter values repectively."
  (interactive
   (when current-prefix-arg
     (list (ido-completing-read
	    "Action: " '("swap" "header" "param")
	    nil nil nil nil "header"))))
  (let ((act (or (and action (intern action)) 'swap)))
    (case act
      ;; swap :parameters and :header args
      (swap
       (org-dp-rewire
	nil t t t nil
	:parameters (lambda (_old_ elem)
		      (org-string-nw-p
		       (mapconcat
			'identity
			(org-element-property :header elem) " ")))
	:header (lambda (_old_ elem)
		  (let ((params
			 (ignore-errors
			   (org-split-string
			    (org-element-property
			     :parameters elem))))
			headers)
		    (while params
		      (setq headers
			    (cons
			     (format "%s %s"
				     (pop params)
				     (pop params))
			     headers)))
		    headers))
	:preserve-indent 1))
      ;; convert :parameters to :header args
      (header
       (org-dp-rewire
	nil t t t nil
	:preserve-indent 1
	:parameters nil
	:header (lambda (_old_ elem)
		  (let ((params
			 (ignore-errors
			   (org-split-string
			    (org-element-property
			     :parameters elem))))
			headers)
			  (while params
			    (setq headers
				  (cons
				   (format "%s %s"
					   (pop params)
					   (pop params))
				   headers)))
		    (append headers _old_)))))
      ;; convert :header args to :parameters
      (param
       (org-dp-rewire
	nil t t t nil
	:preserve-indent 1
	:parameters (lambda (_old_ elem)
		      (concat 
		       (mapconcat
			'identity
			(org-element-property :header elem)
			" ")
		       (or (org-string-nw-p _old_) "")))
	:header nil))
      (t (error "Not a valid action: %s" act)))))

;;; Run Hooks and Provide
;;; org-dp-lib.el ends here

