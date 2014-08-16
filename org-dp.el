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

;;; Functions
;;;; Core Functions

(defun* org-dp-create (elem-type &optional contents insert-p &rest args)
  "Create Org element, maybe insert at point."
  (let* ((type (or elem-type 'headline))
	 (cont (if (consp contents)
		   contents
		 (list 'section nil (or contents ""))))
	 (strg (org-element-interpret-data
		(cons type (cons (car args) (list cont))))))
    (if insert-p (insert strg) strg)))

;; TODO check and delete
;; Note that, if ELEMENT is given and should be replaced, it *must*
;; be a quoted (!) symbol with an Org element's parse-tree as
;; value. This is because after rewiring a parsed *and* assigned Org
;; element its location properties like `:begin', `:end' and
;; `post-affiliated' might have changed and must be updated by
;; parsing the rewired and inserted element-at-point
;; again. Otherwise, if ELEMENT is given and REPLACE is either
;; `nil', `append' or `prepend', it can be given as a nested list
;; too, in form of the elements parse-tree.

(defun* org-dp-rewire (elem-type &optional element contents replace affiliated &rest args)
  "Rewire element-at-point or ELEMENT (if given).

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

ELEM-TYPE is one of the types in `org-element-all-elements'. If
it is nil, the element type of the original element is used. ARGS
is a plist consisting of key-val pairs of all other keyword
arguments given.

The former value of an element property can be reused in the
creation of a new value by giving a `lambda' expession with two
function arguments instead of a value to a key. The first
argument will then be replaced by the property's former value
when applying the function. The second argument should be the
parsed element itself, enabling access to its type and all its
properties inside of the lambda expression."
  (let* ((orig-elem (cond
		     ((and (not (booleanp element))
			   (symbolp element))
		      (eval element))
		     ((stringp element)
		      (let ((el (car (read-from-string element))))
			(when (consp el) el)))
		     ((consp element) element)
		     (t (org-element-at-point))))
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


;;; Run Hooks and Provide

(provide 'org-dp)
;;; org-dp.el ends here
