;;; org-dp.el --- Declarative Programming with Org Elements
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

;; This library introduces a few 'public' functions

;;  - `org-dp-create' :: create a new Org element by building its
;;       internal representation

;;  - `org-dp-rewire' :: modify (and maybe transform) and existing Org
;;       element by changing its internal representation

;;  - `org-dp-map' :: map elements in a buffer and 'rewire' them (not
;;                    yet implemented)

;; and 1 command as generic UI

;;  - `org-dp-prompt' :: universal function for getting user info

;; The following more 'private' functions and commands are used by the
;; core/UI functions, but might be useful by themselves

;;  - `org-dp-contents' :: get content of (local) element

;;  - `org-dp-in' :: return position-info if inside element, nil
;;                    otherwise (not yet implemented)

;;  - `org-dp-prompt-for-src-block-props' :: prompt user for src-block
;;       properties (adapted from ob-core.el)

;; Note that the src-block parameters are appended to the src-block's
;; headline. If you rather want them as separate #+header: lines on
;; top of the src-block you can use `org-dp-toggle-headers' from
;; org-dp-lib.el for swapping headers and parameters.

;;; Requires

(require 'cl)
(require 'ox)

;;; Variables
;;;; Vars
;;;; Consts

(defconst org-dp-elem-props
  (list
   '(center-block . (contents))
   '(drawer . (:drawer-name contents))
   '(dynamic-block . (:block-name :arguments contents))
   '(footnote-definition . (:label contents))
   '(headline . (:level :todo-keyword :priority :title :archivedp
			:tags :commentedp :pre-blank
			:footnote-section-p contents))
   '(inline-task . (:level :todo-keyword :priority :title :tags
			   contents))
   '(item . (:bullet :checkbox :counter :tag contents))
   '(plain-list . (contents))
   '(property-drawer . (contents))
   '(quote-block . (contents))
   '(section . (contents))
   '(special-block . (:type contents))
   '(babel-call . (:value))
   '(clock . (:value :duration))
   '(comment . (:value))
   '(comment-block . (:value))
   '(diary-sexp . (:value))
   '(example-block . (:switches :preserve-indent :value))
   '(fixed-width . (:value))
   '(horizontal-rule . nil)
   '(keyword . (:key :value))
   '(latex-environment . (:value))
   '(node-property . (:key :value))
   '(paragraph . (contents))
   '(planning . (:deadline :scheduled :closed))
   '(src-block . (:language :switches :parameters :value
			    :preserve-indent))
   '(table . (:type :value :tblfm contents)) ; :value when table.el
   '(table-row . (:type contents))
   '(verse-block . (contents)))
  "AList of elements and their interpreted properties.")

(defconst org-dp-inline-elems
  (list 'babel-call 'footnote-definition 'inline-task 'table-cell)
  "List of Org elements and objects that not necessarily start with a newline.")

(defconst org-dp-value-blocks
  (list 'comment-block 'example-block 'src-block)
  "List of Org block that have a :value instead of contents.")

(defconst org-dp-affiliated-keys
  (list :caption :data :header :headers :label :name :plot :resname
	:result :results :source :srcname :tblname)
  "List of all `org-element-affiliated-keywords' as downcased
    keywords, including deprecated old keywords that are mapped
    to new keywords in `org-element-keyword-translation-alist'.")

(defconst org-dp-single-keys
  (list :name :plot :results)
  "Selection of downcased keywords from
  `org-element-affiliated-keywords', namely those new keywords
  not member of `org-dp-multiple-keys', `org-dp-parsed-keys' or
  `org-dp-dual-keys'.")

(defconst org-dp-multiple-keys
  (list :header :caption)
  "Downcased keywords from `org-element-multiple-keywords'.")

(defconst org-dp-parsed-keys
  (list :caption)
  "Downcased keywords from `org-element-parsed-keywords'.")

(defconst org-dp-dual-keys
  (list :caption :results)
  "Downcased keywords from `org-element-dual-keywords'.")

;;;; Customs

;;;;; Custom Groups

(defgroup org-dp nil
  "Declarative Programming with Org Elements."
  :prefix "org-dp"
  :group 'lisp
  :link '(url-link "https://github.com/tj64/org-dp"))

;;;;; Custom Vars

;; (defcustom org-dp-default-babel-lang "emacs-lisp"
;;   "Default Babel language used for new src-blocks."
;;   :group 'org-dp
;;   :type 'string)

;;; Functions
;;;; Core Functions

(defun* org-dp-create (elem-type &optional contents insert-p affiliated &rest args)
  "Create Org element of type ELEM-TYPE (headline by default).

Depending on its type, CONTENTS is used as the element's content
or value. 

If INSERT-P is nil, return interpreted string. If its value is
the symbol 'data', return the raw data, otherwise, for any other
non-nil value, insert interpreted element at point.

AFFILIATED should be a plist of affiliated keys and values if
given.

If ARGS are key-value pairs of (interpreted) properties for
ELEM-TYPE (see `org-dp-elem-props' for a complete overview)."
  (let* ((type (or elem-type 'headline))
	 (val (when (and (memq type org-dp-value-blocks)
			 (not (org-string-nw-p
			       (plist-get args :value))))	
		(list :value (or (org-string-nw-p contents) "\n"))))
	 ;; FIXME kind of a hack (pre-processing really necessary?)
	 (preproc-args (cond
			((and (consp (car args))
			      (consp (caar args)))
			 (caar args))
			((consp (car args)) (car args))
			(t args)))
	 (data (list type
		     (cond
		      ((consp affiliated) (org-combine-plists
					   preproc-args affiliated
					   val))
		      ((not affiliated)
		       (mapcar
			(lambda (--aff-kw)
			  (setq preproc-args
				(plist-put preproc-args
					   --aff-kw nil)))
			(intersection preproc-args
				      org-dp-affiliated-keys))
		       (org-combine-plists preproc-args val))
		      (t (org-combine-plists preproc-args val)))
		     (unless val
		       (if (and (stringp contents)
				(not (memq
				      type
				      org-element-all-objects)))
			   (cons 'section `(nil ,contents))
			 contents)))))
    (cond
     ((eq insert-p 'data) data)
     (insert-p
      (progn
	(unless (and (bolp)
		     (not (memq type org-dp-inline-elems)))
	  (newline))
	(insert (org-element-interpret-data data))))
      (t (org-element-interpret-data data)))))

(defun* org-dp-rewire (elem-type &optional contents replace affiliated &rest args)
  "Rewire element-at-point.

If CONTENTS is non-nil, act conditional on its value:

 - string or internal representation (parse-tree) :: use
      raw/interpreted value as rewired element's contents.

 - function with two arguments :: call function with original
   argument's contents (in parse-tree format) as first argument
   and original element (in parse-tree format) as second
   argument. Use the returned string/list (in parse-tree format)
   as rewired element's raw/interpreted contents.

 - t :: (boolean) get interpreted contents of original element.

If REPLACE is non-nil, act conditional on its value:

 - append :: (symbol) append rewired element after original element

 - prepend :: (symbol) prepend rewired element before original element

 - non-nil :: (any) replace original element with rewired element

 - nil :: just return rewired element

If AFFILIATED is non-nil, act conditional on its value:

 - property list :: (consp) combine element's property list with
                    this plist of affiliated keywords

 - non-nil :: (any) all affiliated keywords are retained in
              rewired element.

 - nil :: (boolean) no affiliated keywords are retained in
          rewired element.

ELEM-TYPE is one of the types in `org-element-all-elements'. If
it is nil, the element type of the original element is used. ARGS
is a plist consisting of key-val pairs of all other keyword
arguments given, defining the (rewired) element's properties.

The former value of an element property can be reused in the
creation of a new value by giving a `lambda' expession or
function taking two arguments (instead of just a value) to a
key. The first argument will then be replaced by the property's
former value when applying the function. The second argument
should be the parsed element itself, enabling access to its type
and all its properties inside of the lambda expression."
  (let* ((orig-elem (org-element-at-point))
	 (type (or elem-type (org-element-type orig-elem)))
	 (elem (copy-list orig-elem))
	 (plist (copy-list (cadr elem)))
	 (beg (set-marker
	       (make-marker) (org-element-property :begin elem)))
	 (paff (set-marker
		(make-marker)
		(org-element-property :post-affiliated elem)))
	 (end (set-marker
	       (make-marker) (org-element-property :end elem)))
	 (cont (let ((orig-elem-cont (org-dp-contents elem)))
		 (cond
		  ((and (consp contents) (functionp contents))
		   (apply contents (list orig-elem-cont elem)))
		  ((and contents (booleanp contents))
		   orig-elem-cont)
		  (t contents))))
	 strg)
    (while args
      (let* ((key (pop args))
	     (val-or-fun (pop args))
	     (old-val (org-element-property key elem))
	     (new-val
	      (if (functionp val-or-fun)
		  (apply val-or-fun old-val (list elem))
		val-or-fun)))
	(setq plist (plist-put plist key new-val))))
    (setq strg (org-element-interpret-data
		(list (or type (org-element-type elem))
		      (cond
		       ((consp affiliated)
			(org-combine-plists plist affiliated))
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
			  (cons 'section `(nil ,cont))
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
	   (save-excursion (insert strg)))))))


(defun org-dp-map ()
  "")

;;;; Utility Functions

(defun org-dp-contents (&optional element interpret-p no-properties-p)
  "Get contents of element-at-point or ELEMENT.
If INTERPRET-P is non-nil, call `org-element-interpret-data' on
return value. Call `org-no-properties' on result if
NO-PROPERTIES-P is non-nil too."
  (let* ((elem (cond
		((and (not (booleanp element))
		      (symbolp element))
		 (eval element))
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
		      (org-element-parse-buffer 'object)
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
	 (header-args ""))
    (while (y-or-n-p "Add arg ")
      (let* ((key (org-icompleting-read
		   "Header Arg: "
		   (mapcar
		    (lambda (header-spec)
		      (symbol-name (car header-spec)))
		    headers)))
	     (vals (cdr (assoc (intern key) headers))))
	(setq header-args
	      (concat
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
			   ;; arg nil)))
		   vals " "))))
	       (if (org-string-nw-p header-args) " " "")
	       header-args))))
    (message "src-block params: %s"
	     (list :language lang :parameters header-args))
    (list :language lang :parameters header-args)))

(defun* org-dp-prompt (&optional elem elem-lst &key noprompt-cont noprompt-val noprompt-replace noprompt-affiliated noprompt-src-block noprompt-args)
  "Prompt user for arguments.

Optional arg ELEM, if given, is the parse-tree of an Org element,
used to derive default values when prompting the user.

Optional arg ELEM-LST, if given, is a subset of
`org-element-all-elements' used for completing-read functions. If
only an atomic symbol is given, that element type is used without
prompting the user.

If any of the

 NOPROMPT-{CONT|VAL|REPLACE|AFFILIATED|SRC-BLOCK|ARGS}

keyword arguments is given, it should either be t, meaning that
prompting the user for that argument will be suppressed and its
value will be nil, or an adecuate type that can be set as the
arguments value:

 - cont :: string

 - val :: string

 - replace :: string (will be interned)

 - affiliated :: plist

 - src-block :: plist

 - args :: plist

The function's return list consists of the following elements:

 (elem-type contents replace affiliated args)"
  (interactive)
  (let* ((elem-type (cond
		     ((and (not (booleanp elem-lst))
			   (symbolp elem-lst)
			   (memq elem-lst org-element-all-elements))
		      elem-lst)
		     ((list elem-lst)
		      (intern (org-completing-read
			       "Element type: "
			       (mapcar
				'symbol-name
				(or elem-lst
				    org-element-all-elements)))))
		     (t (user-error
			 "Not a symbol or list (of elements): %s"
			 elem-lst))))
	 (contents (when (memq 'contents
			       (cdr (assoc elem-type
					   org-dp-elem-props)))
		     (cond
		      (noprompt-cont
		       (org-string-nw-p noprompt-cont))
		      ((and elem
			    (y-or-n-p
			     "Contents - use default value "))
		       (if (memq 'contents
				 (cdr
				  (assoc
				   (org-element-type elem)
				   org-dp-elem-props)))
			   (org-dp-contents elem t)
			 (org-element-property :value elem)))
		      (t (read-string "Element contents: ")))))
	 (value (when (memq :value
			    (cdr (assoc elem-type
					org-dp-elem-props)))
		  (cond
		   (noprompt-val
		    (org-string-nw-p noprompt-val))
		   ((and elem
			 (y-or-n-p
			  ":value - use default "))
		    (if (memq :value
			      (cdr
			       (assoc
				(org-element-type elem)
				org-dp-elem-props)))
			(org-element-property :value elem)
		      (or contents
			  (org-dp-contents elem t))))
		   (t (read-string "Element value: ")))))
	 (replace (if noprompt-replace
		      (org-string-nw-p noprompt-replace)
		    (org-completing-read
		     "Replace? "
		     (mapcar 'symbol-name
			     '(nil t append prepend)))))
	 (arglst (remove 'contents
			 (cdr (assoc elem-type org-dp-elem-props))))
	 affiliated args)
    ;; get affiliated keywords
    (if noprompt-affiliated
	(when (consp noprompt-affiliated)
	  (setq affiliated noprompt-affiliated))
      (let ((branch (org-completing-read
		     "With affiliated keywords "
		     '("nil" "t" "list"))))
	(if (member branch '("nil" "t"))
	    (setq affiliated (intern branch))
	  (mapc
	   (lambda (--aff-key)
	     (if (and (org-element-property --aff-key elem)
		      (y-or-n-p (format "%s - use default value "
					--aff-key)))
		 (setq affiliated
		       (append
			(list --aff-key
			      (org-element-property --aff-key elem))
			affiliated))
	       (setq
		affiliated
		(append
		 (list
		  --aff-key
		  (cond
		   ((memq --aff-key
			  (intersection org-dp-multiple-keys
					org-dp-dual-keys))
		    (let (accum)
		      (while (y-or-n-p
			      (format "%s - add value " --aff-key))
			(setq accum
			      (cons
				(cons
				 (org-string-nw-p
				  (read-string
				   (format " %s value "
					   --aff-key)))
				  (org-string-nw-p
				  (read-string
				   (format " %s dual "
					   --aff-key))))
			       accum)))
		      accum))
		   ((memq --aff-key org-dp-multiple-keys)
		    (let (accum)
		      (while (y-or-n-p
			      (format "%s - add value " --aff-key))
			(setq accum
			      (cons (read-string
				     (format "%s " --aff-key))
				    accum)))
		      (if (consp accum)
			  accum
			(org-string-nw-p accum))))
		   ((memq --aff-key org-dp-dual-keys)
		    (let* ((val (org-string-nw-p
				(read-string
				 (format " %s value " --aff-key))))
			  (dual (when val
				  (org-string-nw-p
				   (read-string
				    (format " %s dual "
					    --aff-key))))))
		      (and val (cons val dual))))
		      ;; (and val dual (cons val dual))))
		   (t (org-string-nw-p
		       (read-string (format "%s " --aff-key))))))
		 affiliated))))
	   (delete-duplicates
	    (append org-dp-single-keys org-dp-multiple-keys
		    org-dp-parsed-keys org-dp-dual-keys))))))
    ;; get src-block parameters
    (if noprompt-src-block
	(when (consp noprompt-src-block)
	  (setq args noprompt-src-block))
      (when (eq elem-type 'src-block)
	(mapc
	 (lambda (--key) (setq arglst (delq --key arglst)))
	 (list :language :parameters))
	(if (and elem
		 (eq (org-element-type elem) 'src-block)
		 (y-or-n-p
		  "Src-block params - use default values "))
	    (setq args
		  (append
		   (list
		    :language (org-element-property :language elem)
		    :parameters (org-element-property
				 :parameters elem))
		   args))
	  (when (y-or-n-p "Provide src-block params ")
	    (setq args
		  (append
		   (call-interactively
		    'org-dp-prompt-for-src-block-props)
		   args))))))
    ;; get element properties
    (if noprompt-args
	(when (consp noprompt-args)
	  (setq args (cons noprompt-args args)))
      ;; maybe unnest args-plist (FIXME hack)
      (when (and args (consp (car args)))
	(setq args (car args)))
      ;; special case value (FIXME prompted when shouldn't)
      (when value
	(setq arglst (delq :value arglst))
	(setq args (plist-put args :value value)))
      (while arglst
	(let ((--prop (pop arglst)))
	  (setq args
		(append
		 (list
		  --prop
		  (if (and elem
			   (memq --prop
				 (cdr
				  (assoc
				   (org-element-type elem)
				   org-dp-elem-props)))
			   (y-or-n-p
			    (format "%s - use default value "
				    --prop)))
		      (org-element-property --prop elem)
		    (read-string (format "%s " --prop))))
		 args)))))
    (message "return-list: %s"
	     (list elem-type contents replace affiliated args))
    (list elem-type contents replace affiliated args)))

;;; Run Hooks and Provide

(provide 'org-dp)
;;; org-dp.el ends here
