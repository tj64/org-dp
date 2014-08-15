;;; org-dp.el --- Declarative Programming with Org Elements
;; Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
;; Version: 0.9
;; URL: https://github.com/tj64/puml

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

;; Functions for declarative programming with Org elements. They allow
;; to declare what should be done and leave the low-level work, the
;; "how-to", to the Org parser/interpreter framework.

;; With other words, org-dp acts on the internal representation of Org
;; elements rather than on their textual representation, and leaves
;; the transformation between both representations to the
;; parser/interpreter framework. To create or modify an element, you
;; call the parser to open it up, rewire its internals, and then call
;; the interpreter to build the element again based on its modified
;; internals.

;; Since all Org elements are uniformely represented as nested lists
;; internally, with their properties stored as key-val pairs in
;; plists, they can be treated in a much more uniform way when dealing
;; with the internal representation instead of the highly variable
;; textual representations. A big advantage of plists is that only
;; those properties that are actually accessed matter, so when
;; transforming one Org element into another on the internal level one
;; does not have to worry about not matching properties as long as
;; these are not used by the interpreter when building the textual
;; representation of the transformed element.

;;;; Usage

;; This library introduces 3 'public' functions

;;  - `org-dp-create' :: create a new Org element by building its
;;       internal representation

;;  - `org-dp-rewire' :: modify (and maybe transform) and existing Org
;;       element by changing its internal representation

;;  - `org-dp-map' :: map elements in a buffer and 'rewire' them (not
;;                    yet implemented)


;; and 1 'private' function

;;  - `org-dp-contents' :: get content of (local) element

;; The utility commands included in the library are not part of the
;; library but rather application examples. They can safely be moved
;; to more appropriate places.

;;; Requires

(require 'cl)
(require 'ox)

;;; Variables
;;;; Vars
;;;; Consts

(defconst org-dp-affiliated-keys
  (list :caption :data :header :headers :label :name :plot :resname
	:result :results :source :srcname :tblname)
  "List of `org-element-affiliated-keywords' as downcased
    keywords.")

(defconst org-dp-interpreted-keys-alist
  (list
   '(center-block ((contents . (consp stringp))))
   '(drawer ((:drawer-name . stringp)
	     (contents . consp)))
   '(dynamic-block ((:block-name . stringp)
		    (:arguments . stringp)
		    (contents . (consp stringp))))
   '(footnote-definition ((:label . stringp)
			  (contents . (consp stringp))))
   '(headline ((:level . (1 2 3 4 5 6 7 8))
	       (:todo-keyword . org-todo-keyword-alist-for-agenda)
	       (:priority . (?A ?B ?C))
	       (:title . consp)
	       (:archivedp . symbolp)
	       (:tags . consp)
	       (:commentedp . booleanp)
	       (:pre-blank . numberp)
	       (:footnote-section-p . booleanp)
	       (contents . (consp stringp))))
   '(inline-task ((:level . (1 2 3 4 5 6 7 8))
		  (:todo-keyword
		   . org-todo-keyword-alist-for-agenda)
		  (:priority . (?A ?B ?C))
		  (:title . consp)
		  (:tags . consp)
		  (contents . (consp stringp))))
   '(item ((:bullet . stringp)
	   (:checkbox . stringp)
	   (:counter . numberp)
	   (:tag . stringp)
	   (contents . (consp stringp))))
   '(plain-list ((contents . (consp stringp))))
   '(property-drawer ((contents .(consp stringp))))
   '(quote-block ((contents . (consp stringp))))
   '(section ((contents . (consp stringp))))
   '(special-block ((:type . stringp)
		    (contents . (consp stringp))))
   '(babel-call ((:value . stringp)))
   '(clock ((:value . stringp)
	    (:duration . stringp)))
   '(comment ((:value . stringp)))
   '(comment-block ((:value . stringp)))
   '(diary-sexp ((:value . stringp)))
   '(example-block ((:switches . stringp)
		    (:preserve-intent . numberp) ; FIXME number?
		    (:value . stringp)))
   '(fixed-width ((:value . stringp)))
   '(horizontal-rule (()))
   '(keyword ((:key . stringp)
	      (:value . stringp)))
   '(latex-environment ((:value . stringp)))
   '(node-property ((:key . stringp)
		    (:value . stringp)))
   '(paragraph ((contents . (consp stringp))))
   '(planning ((:deadline . stringp)
	       (:scheduled . stringp)
	       (:closed . stringp)))
   '(scr-block ((:language . stringp)
		(:switches . stringp)
		(:parameters . stringp)
		(:value . stringp)
		(:preserve-indent . numberp))) ; FIXME number?
   '(table ((:type . symbolp)
	    (:value . stringp)
	    (:tblfm . consp)))
   '(table-row ((:type . symbolp)
		(contents . (consp stringp))))
   '(quote-block ((contents . (consp stringp)))))
  "AList of elements and interpreted keywords with
  type-information.")

(defconst org-dp-lorem-objects
  (list
   '(bold . "*lorem*")
   '(code . "~lorem~")
   '(entity . "\\emph{}")
   '(export-snippet . "@@ascii:lorem@@")
   '(footnote-reference . "[fn::lorem]")
   '(inline-babel-call . "call_lorem()")
   '(inline-src-block . "src_emacs-lisp[:var x=lorem]{x}")
   '(italic . "/lorem/")
   '(latex-fragment
     . "\begin{equation}\nlorem=\sqrt{x}\n\end{equation}")
   '(line-break . "lorem\\\\\nipsum")
   '(link-interpreter . "[[http://orgmode.org/] [Lorem]]")
   '(macro . "{{{lorem(arg1, arg2)}}}")
   '(radio-target . "<<<lorem>>>")
   '(statistics-cookie . "[33%]")
   '(strike-through . "+lorem+")
   '(subscript . "_{lorem}")
   '(superscript . "^{lorem}")
   '(table-cell . " lorem |")
   '(target . "<<lorem>>")
   '(timestamp . "<2014-08-14 Do 20:53>")
   '(underline . "_lorem_")
   '(verbatim . "=lorem="))
  "AList of object types and examples.")

;;; Functions
;;;; Core Functions

(defun* org-dp-create (elem-type &optional contents insert-p &rest args &allow-other-keys)
  "Create Org element, maybe insert at point."
  (let* ((type (or elem-type 'headline))
	 (cont (if (consp contents)
		   contents
		 (list 'section nil `(paragraph nil ,contents))))
	 (strg (org-element-interpret-data
 	       (list type args cont))))
    (if insert-p (insert strg) strg)))

(defun* org-dp-rewire (elem-type &optional element contents replace affiliated &rest args &allow-other-keys)
  "Rewire element-at-point or ELEMENT (if given).

Note that, if ELEMENT is given and should be replaced, it *must*
be a quoted (!) symbol with an Org element's parse-tree as
value. This is because after rewiring a parsed *and* assigned Org
element its location properties like `:begin', `:end' and
`post-affiliated' might have changed and must be updated by
parsing the rewired and inserted element-at-point
again. Otherwise, if ELEMENT is given and REPLACE is either
`nil', `append' or `prepend', it can be given as a nested list
too, in form of the elements parse-tree.

If CONTENT is non-nil, act conditional on its value:

 - string or internal representation (parse-tree) :: use
      raw/interpreted value as rewired element's contents.

 - function with two arguments :: call function with original
   argument's contents (in parse-tree format) as first argument
   and original element (in parse-tree format) as second
   argument. Use the returned string/list (in parse-tree format)
   as rewired element's raw/interpreted contents.

 - t :: (boolean) get interpreted contents of original element.

Act conditional on value of REPLACE:

 - append :: (symbol) append rewired element after original element

 - prepend :: (symbol) prepend rewired element before original element

 - non-nil :: (any) replace original element with rewired element

 - nil :: just return rewired element

Act conditional on value of AFFILIATED:

 - list of keywords :: (consp) properties of the original element
      whose keys are member (memq) of this list (of downcased
      keywords from `org-element-affiliated-keywords') are
      retained in the rewired element.

 - non-nil :: (any) all affiliated keywords are retained in
              rewired element.

 - nil :: (boolean) no affiliated keywords are retained in
          rewired element.

ELEM-TYPE is one of the types in `org-element-all-elements'. ARGS is a
plist consisting of key-val pairs of all other keyword arguments
given. 

The former value of an element property can be reused in the
creation of a new value by giving a `lambda' expession with two
function arguments instead of a value to a key. The first
argument will then be replaced by the property's former value
when applying the function. The second argument should be the
parsed element itself, enabling access to its type and all its
properties inside of the lambda expression."
  (let* ((type (or elem-type (headline)))
	 (orig-elem (cond
		     ((and (not (booleanp element))
			   (symbolp element))
		      (eval element))
		     ((stringp element)
		      (let ((el (car (read-from-string element))))
			(when (consp el) el)))
		     ((consp element) element)
		     (t (org-element-at-point))))
	 (elem (copy-list orig-elem))
	 (plist (copy-list (cadr elem)))
	 (beg (set-marker
	       (make-marker) (org-element-property :begin elem)))
	 (paff (set-marker
		(make-marker)
		(org-element-property :post-affiliated elem)))
	 (end (set-marker
	       (make-marker) (org-element-property :end elem)))
	 (cont (cond
		((and (consp contents) (functionp contents))
		 (apply 'contents (list elem)))
		((and contents (booleanp contents))
		 (org-dp-contents elem))
		(t contents)))
	 strg)
    (while args
      (if (eq (car args) :type)
	  (dotimes (i 2) (pop args))
	(let* ((key (pop args))
	       (val-or-fun (pop args))
	       (old-val (org-element-property key elem))
	       (new-val
		(if (functionp val-or-fun)
		    (apply val-or-fun old-val (list elem))
		  val-or-fun)))
	  (setq plist (plist-put plist key new-val)))))
    (setq strg (org-element-interpret-data
		(list (or type (org-element-type elem))
		      (cond
		       ((consp affiliated)
			(mapcar
			 (lambda (--aff-kw)
			   (setq plist (plist-put
					plist --aff-kw nil)))
			 (intersection plist
				       (set-difference
					org-dp-affiliated-keys
					affiliated)))
			plist)
		       ((not affiliated)
			(mapcar
			 (lambda (--aff-kw)
			   (setq plist (plist-put
					plist --aff-kw nil)))
			 (intersection plist
				       org-dp-affiliated-keys))
			plist)
		       (t plist))
		      (if (stringp cont)
			  (list 'section nil `(paragraph nil ,cont))
			cont))))
    (case replace
      (append (save-excursion (goto-char end) (insert strg)))
      (prepend (goto-char beg) (insert strg))
      (t (if (not replace)
	     strg
	   (delete-region beg end)
	   (goto-char end)
	   (set-marker beg nil)
	   (set-marker paff nil)
	   (set-marker end nil)
	   (save-excursion (insert strg))
	   ;; (message "Before:\nsym: %s\nsym-name: %s\nsym-val: %s\n"
	   ;; 	    element
	   ;; 	    (ignore-errors (symbol-name element))
	   ;; 	    (ignore-errors (symbol-value element)))
	   ;; (if (and element (not (symbolp element)))
	   ;;     (user-error
	   ;; 	 "Argument ELEMENT not a quoted symbol:\n%s"
	   ;; 	 element)
	   ;;   (set element (org-element-at-point))
	   ;;   ;; (message
	   ;;   ;;  "After:\nsym: %s\nsym-name: %s\nsym-val: %s\n"
	   ;;   ;; 	      element
	   ;;   ;; 	      (ignore-errors (symbol-name element))
	   ;;   ;; 	      (ignore-errors (symbol-value element)))
	   ;;   )
	   )))))





(defun org-dp-map ()
  "")

;;;; Utility Functions

(defun org-dp-contents (&optional element interpret-p no-properties-p)
  "Get contents of element-at-point or ELEMENT.
If INTERPRET-P is non-nil, call `org-element-interpret-data' on
return value. Call `org-no-properties' on result if
NO-PROPERTIES-P is non-nil too."
  (let* ((elem (cond
		((symbolp element) (eval element))
		((stringp element)
		 (let ((el (car (read-from-string element))))
		   (when (consp el) el)))
		((consp element) element)
		(t (org-element-at-point))))
	 (type (org-element-type elem)))
    (save-restriction
      (narrow-to-region 
       (org-element-property :begin elem)
       (org-element-property :end elem))
      (let ((cont (org-element-map
		      (org-element-parse-buffer 'object
						'visible-only)
		      type 'org-element-contents nil t)))
	(cond
	 ((and interpret-p no-properties-p)
	  (org-no-properties (org-element-interpret-data cont)))
	 (interpret-p
	  (org-element-interpret-data cont))
	 (t cont))))))


(defun org-dp-in (type)
  "")

;;; Commands

;;;; Prompt User

;; This function reuses parts of`org-babel-insert-header-arg' and
;; `org-babel-demarcate-block'
(defun org-dp-prompt-for-src-block-props (lang)
  "Prompt for src-block header argument.
Select from lists of common args and values. Argument LANG
specifies the Org Babel language."
  (interactive
   (list (org-icompleting-read
	  "Lang: "
	  (mapcar #'symbol-name
		  (delete-dups
		   (append (mapcar #'car
				   org-babel-load-languages)
			   (mapcar
			    (lambda (el) (intern (car el)))
			    org-src-lang-modes)))))))
  (let* ((lang-headers (intern
			(concat "org-babel-header-args:" lang)))
	 (headers (org-babel-combine-header-arg-lists
		   org-babel-common-header-args-w-values
		   (when (boundp lang-headers)
		     (eval lang-headers))))
	 header-args)
    (while (y-or-n-p "Add arg ")
      (let* ((key (org-icompleting-read
		   "Header Arg: "
		   (mapcar
		    (lambda (header-spec)
		      (symbol-name (car header-spec)))
		    headers)))
	     (vals (cdr (assoc (intern key) headers))))
	(setq header-args
	      (cons
	       (format
		":%s %s"
		key
		(cond
		 ((eq vals :any)
		  (read-from-minibuffer "value: "))
		 ((listp vals)
		  (mapconcat
		   (lambda (group)
		     (let ((arg (org-icompleting-read
				 "Value: "
				 (cons "default"
				       (mapcar #'symbol-name
					       group)))))
		       (if (and arg
				(not (string= "default" arg)))
			   arg "")))
		   vals ""))))
	       header-args))))
    (list :language lang :header header-args)))

(defun org-dp-prompt ()
  "Prompt user for arguments.
Return list consists of the following elements:
  (element contents type replace affiliated args)"
  (interactive
   (let* ((type (org-completing-read "Element type (symbol): "
				     org-element-all-elements))
	  
		 nil nil nil nil "emacs-lisp"))


	  (ido-read-block-type '(ido-completing-read
				 "Block Type: " all-types
				 nil nil nil nil "emacs-lisp"))
	  (ido-read-headers '(while (y-or-n-p "Add header ")
			       (cons
				(ido-completing-read
				 "arg: "
				 (mapcar
				  'symbol-name
				  org-babel-header-arg-names)
				 nil nil nil nil "var")
				(read-string "value: "))))
	  (read-dyn-params '(while (y-or-n-p "Add parameter ")
			      (cons
			       (read-string "param: ")
			       (read-string "value: "))))))


;;;; Library of Org Commands

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
		 (save-excursion
		   (goto-char (or (car dblock-limits)
				  src-block-beg
				  (car block-limits)))
		   (let* ((parsed-block (org-element-at-point))
			  (type (org-element-type parsed-block))
			  (val-or-cont
			   (if (or
				(eq type 'verse-block)
				(memq type
				      org-element-greater-elements))
			       (org-dp-contents parsed-block t)
			     (org-element-property :value
						   parsed-block))))
		     (insert
		      (org-element-paragraph-interpreter
		       (org-dp-rewire parsed-block t nil nil
					  :type 'paragraph)
		       val-or-cont)))
		   (user-error "Unwrapped block content.")))
		;; replace surrounding block
		((y-or-n-p "Replace surrounding block ")
		 (save-excursion
		   (goto-char (or (car dblock-limits)
				  src-block-beg
				  (car block-limits)))
		   (let* ((parsed-block
			   (org-element-at-point))
			  (type (org-element-type parsed-block)))
		     (org-dp-rewire parsed-block t nil nil
					;; TODO prompt user!
					:type 'paragraph))
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

(defun org-dp-lorem-ipsum (&optional number-of-entries)
  "Create random Org document with N entries."
  (let ((N (or number-of-entries 100))
	(elems org-dp-interpreted-keys-alist)
	(objs org-dp-lorem-objects)
	last-level rnd strg)
    (with-current-buffer (get-buffer-create "*Org Lorem Ipsum*")
      (dotimes (i N strg)
	(setq rnd (1+ (random 100)))
	(org-dp-create
	 nil
	 (if (< rnd 50) )
		       
	 
	 )))))

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
	nil t t t
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
	:preserve-indent 1
	:type 'src-block))
      ;; convert :parameters to :header args
      (header
       (org-dp-rewire
	nil t t t
	:type 'src-block
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
	nil t t t
	:type 'src-block
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

(provide 'org-dp)
;;; org-dp.el ends here
