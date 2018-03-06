;;; org-dp.el --- Declarative Local Programming with Org Elements
;; Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
;; Version: 1.0
;; URL: https://github.com/tj64/org-dp
;; Package-Requires: ((cl-lib "0.5"))

;;;; MetaData
;;   :PROPERTIES:
;;   :copyright: Thorsten Jolitz
;;   :copyright-years: 2014+
;;   :version:  1.0
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

;; Functions for declarative local programming with Org elements. They
;; allow to declare what should be done and leave the low-level work,
;; the "how-to", to the Org parser/interpreter framework.

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

;; Library org-dp is meant for programming at the local level,
;; i.e. without any (contextual) information except those about the
;; parsed element at point. It is designed to make using the Org-mode
;; parser/interpreter framework at local level as convenient as using
;; it at the global level (with a complete parse-tree produced by
;; `org-element-parse-buffer` available). It takes care of the
;; org-element caching mechanism in that it only acts on copies of the
;; locally parsed elements at point, never on the original parsed (and
;; cached) object itself.

;;;; Usage

;; This library introduces a few 'public' functions

;;  - `org-dp-create' :: create a new Org element by building its
;;       internal representation

;;  - `org-dp-rewire' :: modify (and maybe transform) an existing Org
;;       element by changing its internal representation

;;  - `org-dp-map' :: map elements in a buffer and 'rewire' them

;; and 1 command as generic UI

;;  - `org-dp-prompt' :: universal function for getting user info

;; The following more 'private' functions and commands are used by the
;; core/UI functions, but might be useful by themselves

;;  - `org-dp-contents' :: get content of (local) element

;;  - `org-dp-in' :: return position-info if inside element, nil
;;                    otherwise (not yet implemented)

;;  - `org-dp-prompt-all' :: workhorse function for prompting the user

;;  - `org-dp-prompt-for-src-block-props' :: prompt user for src-block
;;       properties (adapted from ob-core.el)


;; Note that the src-block parameters are appended to the src-block's
;; headline. If you rather want them as separate #+header: lines on
;; top of the src-block you can use `org-dp-toggle-headers' from
;; org-dp-lib.el for swapping headers and parameters.

;;;; Examples 

;;;;; Create Src-Block

;; : #+BEGIN_SRC emacs-lisp
;; :   (org-dp-create 'src-block nil nil
;; :                  '(:name "ex1" :header (":cache no" ":noweb yes"))
;; :                  :language "picolisp"
;; :                  :preserve-indent 1
;; :                  :parameters ":results value"
;; :                  :value "(+ 2 2)")
;; : #+END_SRC

;; #+results:
;; : #+NAME: ex1
;; : #+HEADER: :noweb yes
;; : #+HEADER: :cache no
;; : #+BEGIN_SRC picolisp :results value
;; : (+ 2 2)
;; : #+END_SRC


;;;;; Transform Src-Block into Example Block

;; : #+NAME: ex2
;; : #+HEADER: :results raw
;; : #+BEGIN_SRC emacs-lisp  :exports both
;; :  (org-dp-rewire 'example-block) 
;; : #+END_SRC

;; : #+results: ex2
;; : #+BEGIN_EXAMPLE
;; : (org-dp-rewire 'example-block) 
;; : #+END_EXAMPLE


;;;;; Transform Src-Block into Headline

;; : #+NAME: ex2
;; : #+HEADER: :results raw
;; : #+BEGIN_SRC emacs-lisp :cache no :noweb yes
;; :   (org-dp-rewire 'headline
;; :                  (lambda (_cont_ elem)
;; :                    (concat
;; :                     "This was an\n\n"
;; :                     (org-element-property :language elem)
;; :                     "\n\nsrc-block with header args\n\n"
;; :                     (org-element-property :parameters elem)
;; :                     "\n\nbefore."))
;; :                     'append '(:name "transformed Src-Block")
;; :                     :level 1
;; :                     :title (lambda (_old_ elem)
;; :                              (mapconcat
;; :                               'upcase
;; :                               (split-string
;; :                                (car
;; :                                 (org-element-property :header elem))
;; : 				":")
;; :                               " "))
;; :                     :tags (lambda (_old_ elem)
;; :                             (list (org-element-property :name elem)))
;; : 		    :header nil)
;; : #+END_SRC

;; : #+NAME: transformed Src-Block
;; : *  RESULTS RAW :ex2:
;; : This was an
;; : 
;; : emacs-lisp
;; : 
;; : src-block with header args
;; : 
;; : :cache no :noweb yes
;; : 
;; : before.

;;; Requires

;; (eval-when-compile
;;   (require 'cl))
(require 'cl-lib)
(require 'ox)
(require 'tempo)

;;; Variables
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

(defconst org-dp-no-content-elems
  (list 'babel-call 'clock 'comment 'comment-block 'diary-sexp
	'example-block 'fixed-width 'horizontal-rule 'keyword
	'latex-environment 'node-property 'planning 'src-block)
  "List of Org elements without interpreted .")

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

(defconst org-dp-apply-funs '(create rewire)
  "Functions that can be applied in `org-dp-apply'.")

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

;;;; Vars

(defvar org-dp-tempo-elem-type ""
   "Variable used to store user input in tempo templates.
Input is the Org Element type, e.g. 'src-block.")

(defvar org-dp-tempo-create-with-comments-elem-props
  '(case (intern org-dp-tempo-elem-type)
     (drawer
      '(l ":drawer-name " "\"" p "\"" n> ))
     (dynamic-block 
      '(l ":block-name " "\"" p "\"" n>
	  ":arguments " "\"" p "\"" n>))
     (footnote-definition 
      '(l ":label " "\"" p "\"" n> ))
     (headline 
      '(l ":level " p "1 ;1..8" n>
	  ":priority " p "nil ;65|66|67"  n>
	  ":todo-keyword " p "TODO" n>
	  ":title " "\"" p "\"" n>
	  ":tags " "'(" p " ) ;(\"tag1\" \"tag2\")" n>
	  ":commentedp " p "nil ;t" n>
	  ":pre-blank " p "0" n>
	  ":footnote-section-p " p "nil ;t" n>))
     (inline-task 
      '(l ":level " p "1 ;1..8" n>
	  ":priority " p "nil ;65|66|67"  n>
	  ":todo-keyword " p "TODO" n>
	  ":title " "\"" p "\"" n>
	  ":tags " "'(" p " ) ;(\"tag1\" \"tag2\")" n>))
     (item 
      '(l ":bullet " "\"" p "\"" n> 
	  ":checkbox " "\"" p "\"" n>
	  ":counter " "\"" p "\"" n>
	  ":tag " "\"" p "\"" n> ))
     (special-block 
      '(l ":type " "\"" p "\"" n> ))
     (babel-call 
      '(l ":value " "\"" p "\"" n> ))
     (clock 
      '(l ":value " "\"" p "\"" n> 
	  ":duration " "\"" p "\"" n>))
     (comment 
      '(l ":value " "\"" p "\"" n> ))
     (comment-block 
      '(l ":value " "\"" p "\"" n> ))
     (diary-sexp 
      '(l ":value " "\"" p "\"" n> ))
     (example-block 
      '(l ":switches " "\"" p "\"" n> 
	  ":preserve-indent " "\"" p "\"" n>
	  ":value " "\"" p "\"" n> ))
     (fixed-width 
      '(l ":value " "\"" p "\"" n> ))
     (keyword 
      '(l ":key " "\"" p "\"" n> 
	  ":value " "\"" p "\"" n>))
     (latex-environment 
      '(l ":value " "\"" p "\"" n> ))
     (node-property 
      '(l ":key " "\"" p "\"" n> 
	  ":value " "\"" p "\"" n>))
     (planning 
      '(l ":deadline " "\"" p "\"" n> 
	  ":scheduled " "\"" p "\"" n>
	  ":closed " "\"" p "\"" n>))
     (src-block 
      '(l ":language " "\"" p "\"" n> 
	  ":switches " "\"" p "\"" n>
	  ":parameters " "\"" p "\"" n>
	  ":value " "\"" p "\"" n>
	  ":preserve-indent " "\"" p "\"" n>))
     (table 
      '(l ":type " "\"" p "\"" n> 
	  ":value " "\"" p "\"" n>
	  ":tblfm " "\"" p "\"" n>))
     (table-row 
      '(l ":type " "\"" p "\"" n> ))
     ;; plain-list, property-drawer, quote-block, section,
     ;; horizontal rule, paragraph
     (t nil))   
  "Variable holding element properties for org-dp-create tempo
  template.")

(defvar org-dp-tempo-create-elem-props
  '(case (intern org-dp-tempo-elem-type)
     (drawer
      '(l ":drawer-name " "\"" p "\"" n> ))
     (dynamic-block 
      '(l ":block-name " "\"" p "\"" n>
	  ":arguments " "\"" p "\"" n>))
     (footnote-definition 
      '(l ":label " "\"" p "\"" n> ))
     (headline 
      '(l ":level " p "1 ;1..8" n>
	  ":priority " p "nil ;65|66|67"  n>
	  ":todo-keyword " p "TODO" n>
	  ":title " "\"" p "\"" n>
	  ":tags " "'(" p " )" n>
	  ":commentedp " p "nil" n>
	  ":pre-blank " p "0" n>
	  ":footnote-section-p " p "nil" n>))
     (inline-task 
      '(l ":level " p "1 ;1..8" n>
	  ":priority " p "nil ;65|66|67"  n>
	  ":todo-keyword " p "TODO" n>
	  ":title " "\"" p "\"" n>
	  ":tags " "'(" p " )" n>))
     (item 
      '(l ":bullet " "\"" p "\"" n> 
	  ":checkbox " "\"" p "\"" n>
	  ":counter " "\"" p "\"" n>
	  ":tag " "\"" p "\"" n> ))
     (special-block 
      '(l ":type " "\"" p "\"" n> ))
     (babel-call 
      '(l ":value " "\"" p "\"" n> ))
     (clock 
      '(l ":value " "\"" p "\"" n> 
	  ":duration " "\"" p "\"" n>))
     (comment 
      '(l ":value " "\"" p "\"" n> ))
     (comment-block 
      '(l ":value " "\"" p "\"" n> ))
     (diary-sexp 
      '(l ":value " "\"" p "\"" n> ))
     (example-block 
      '(l ":switches " "\"" p "\"" n> 
	  ":preserve-indent " "\"" p "\"" n>
	  ":value " "\"" p "\"" n> ))
     (fixed-width 
      '(l ":value " "\"" p "\"" n> ))
     (keyword 
      '(l ":key " "\"" p "\"" n> 
	  ":value " "\"" p "\"" n>))
     (latex-environment 
      '(l ":value " "\"" p "\"" n> ))
     (node-property 
      '(l ":key " "\"" p "\"" n> 
	  ":value " "\"" p "\"" n>))
     (planning 
      '(l ":deadline " "\"" p "\"" n> 
	  ":scheduled " "\"" p "\"" n>
	  ":closed " "\"" p "\"" n>))
     (src-block 
      '(l ":language " "\"" p "\"" n> 
	  ":switches " "\"" p "\"" n>
	  ":parameters " "\"" p "\"" n>
	  ":value " "\"" p "\"" n>
	  ":preserve-indent " "\"" p "\"" n>))
     (table 
      '(l ":type " "\"" p "\"" n> 
	  ":value " "\"" p "\"" n>
	  ":tblfm " "\"" p "\"" n>))
     (table-row 
      '(l ":type " "\"" p "\"" n> ))
     ;; plain-list, property-drawer, quote-block, section,
     ;; horizontal rule, paragraph
     (t nil))   
  "Variable holding element properties for org-dp-create tempo
  template.")

(defvar org-dp-tempo-rewire-elem-props
  '(case (intern org-dp-tempo-elem-type)
     (drawer
      '(l ":drawer-name '(lambda (old elem) (" p " ))" n> ))
     (dynamic-block 
      '(l ":block-name '(lambda (old elem) " p " )" n>
	  ":arguments '(lambda (old elem) " p " )" n>))
     (footnote-definition 
      '(l ":label '(lambda (old elem) " p " )" n> ))
     (headline 
      '(l ":level '(lambda (old elem) " p " )" n>
	  ":priority '(lambda (old elem) " p " )" n>
	  ":todo-keyword '(lambda (old elem) " p " )" n>
	  ":title '(lambda (old elem) " p " )" n>
	  ":tags '(lambda (old elem) " p " )" n>
	  ":commentedp '(lambda (old elem) " p " )" n>
	  ":pre-blank '(lambda (old elem) " p " )" n>
	  ":footnote-section-p '(lambda (old elem) " p " )" n>))
     (inline-task 
      '(l ":level '(lambda (old elem) " p " )" n> 
	  ":priority '(lambda (old elem) " p " )" n>
	  ":todo-keyword '(lambda (old elem) " p " )" n>
	  ":title '(lambda (old elem) " p " )" n>
	  ":tags '(lambda (old elem) " p " )" n> ))
     (item 
      '(l ":bullet '(lambda (old elem) " p " )" n> 
	  ":checkbox '(lambda (old elem) " p " )" n>
	  ":counter '(lambda (old elem) " p " )" n>
	  ":tag '(lambda (old elem) " p " )" n> ))
     (special-block 
      '(l ":type '(lambda (old elem) " p " )" n> ))
     (babel-call 
      '(l ":value '(lambda (old elem) " p " )" n> ))
     (clock 
      '(l ":value '(lambda (old elem) " p " )" n> 
	  ":duration '(lambda (old elem) " p " )" n>))
     (comment 
      '(l ":value '(lambda (old elem) " p " )" n> ))
     (comment-block 
      '(l ":value '(lambda (old elem) " p " )" n> ))
     (diary-sexp 
      '(l ":value '(lambda (old elem) " p " )" n> ))
     (example-block 
      '(l ":switches '(lambda (old elem) " p " )" n> 
	  ":preserve-indent '(lambda (old elem) " p " )" n>
	  ":value '(lambda (old elem) " p " )" n> ))
     (fixed-width 
      '(l ":value '(lambda (old elem) " p " )" n> ))
     (keyword 
      '(l ":key '(lambda (old elem) " p " )" n> 
	  ":value '(lambda (old elem) " p " )" n>))
     (latex-environment 
      '(l ":value '(lambda (old elem) " p " )" n> ))
     (node-property 
      '(l ":key '(lambda (old elem) " p " )" n> 
	  ":value '(lambda (old elem) " p " )" n>))
     (planning 
      '(l ":deadline '(lambda (old elem) " p " )" n> 
	  ":scheduled '(lambda (old elem) " p " )" n>
	  ":closed '(lambda (old elem) " p " )" n>))
     (src-block 
      '(l ":language '(lambda (old elem) " p " )" n> 
	  ":switches '(lambda (old elem) " p " )" n>
	  ":parameters '(lambda (old elem) " p " )" n>
	  ":value '(lambda (old elem) " p " )" n>
	  ":preserve-indent '(lambda (old elem) " p " )" n>))
     (table 
      '(l ":type '(lambda (old elem) " p " )" n> 
	  ":value '(lambda (old elem) " p " )" n>
	  ":tblfm '(lambda (old elem) " p " )" n>))
     (table-row 
      '(l ":type '(lambda (old elem) " p " )" n> ))
     ;; plain-list, property-drawer, quote-block, section,
     ;; horizontal rule, paragraph
     (t nil))   
  "Variable holding element properties for org-dp-rewire tempo
  template.")

(defvar org-dp-tempo-cont ""
  "Variable holding element content for tempo templates.")


;;; Functions
;;;; Core Functions

(cl-defun org-dp-create (elem-type &optional contents insert-p affiliated &rest args)
  "Create Org element of type ELEM-TYPE (headline by default).

Depending on its type, CONTENTS is used as the element's content
or value.

If INSERT-P is nil, return interpreted string. If its value is
the symbol 'data', return the raw data, otherwise, for any other
non-nil value, insert interpreted element at point.

AFFILIATED should be a plist of affiliated keys and values if
given.

If ARGS are given, they should be key-value pairs
of (interpreted) properties for ELEM-TYPE (see
`org-dp-elem-props' for a complete overview)."
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
		       (mapc
			(lambda (--aff-kw)
			  (setq preproc-args
				(plist-put preproc-args
					   --aff-kw nil)))
			(cl-intersection preproc-args
					 org-dp-affiliated-keys))
		       (org-combine-plists preproc-args val))
		      (t (org-combine-plists preproc-args val)))
		     (unless val
		       ;; (cond
		       ;; 	((and (stringp contents)
		       ;; 	      (eq type 'headline))
		       ;; 	   (cons 'section `(nil ,contents)))
		       ;; 	((and (stringp contents)
		       ;; 		(not (memq
		       ;; 		      type
		       ;; 		      org-element-all-objects)))
		       ;; 	   (cons 'paragraph `(nil ,contents))) 
		       ;; 	(t contents))
		       (if (and (stringp contents)
				(memq type
				      '(item footnote-definition)))
			   (cons 'paragraph `(nil ,contents))
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

;; (defun* org-dp-create (elem-type &optional contents insert-p affiliated &rest args)
;;   "Create Org element of type ELEM-TYPE (headline by default).
;; Depending on its type, CONTENTS is used as the element's content
;; or5 value. If INSERT-P is non-nil, insert interpreted element at
;; point. AFFILIATED should be a plist of affiliated keys and values
;; if given. ARGS are key-value pairs of (interpreted) properties for
;; ELEM-TYPE (see `org-dp-elem-props' for a complete overview)."
;;   (let* ((type (or elem-type 'headline))
;; 	 (val (when (and (memq type org-dp-value-blocks)
;; 			 (not (org-string-nw-p
;; 			       (plist-get args :value))))	
;; 		(list :value (or (org-string-nw-p contents) "\n"))))
;; 	 ;; FIXME kind of a hack (pre-processing really necessary?)
;; 	 (preproc-args (cond
;; 		      ((and (consp (car args))
;; 			    (consp (caar args)))
;; 		       (caar args))
;; 		      ((consp (car args)) (car args))
;; 		      (t args)))
;; 	 (strg (org-element-interpret-data
;; 		(list type
;; 		      (cond
;; 		       ((consp affiliated) (org-combine-plists
;; 					    preproc-args affiliated
;; 					    val))
;; 		       ((not affiliated)
;; 			(mapcar
;; 			 (lambda (--aff-kw)
;; 			   (setq preproc-args
;; 				 (plist-put preproc-args
;; 					    --aff-kw nil)))
;; 			 (intersection preproc-args
;; 				       org-dp-affiliated-keys))
;; 			(org-combine-plists preproc-args val))
;; 		       (t (org-combine-plists preproc-args val)))
;; 		      (unless val
;; 			(if (and (stringp contents)
;; 				 (not (memq
;; 				       type
;; 				       org-element-all-objects)))
;; 			    (cons 'section `(nil ,contents))
;; 			  contents))))))
;;     (if insert-p
;; 	(progn
;; 	  (unless (and (bolp)
;; 		       (not (memq type org-dp-inline-elems)))
;; 	    (newline))
;; 	  (insert strg))
;;       strg)))

(cl-defun org-dp-rewire (elem-type &optional contents replace affiliated element &rest args)
  "Rewire element-at-point or ELEMENT (if given).

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
	 (elem (cl-copy-list orig-elem))
	 (plist (cl-copy-list (cadr elem)))
	 (beg (set-marker
	       (make-marker) (org-element-property :begin elem)))
	 (paff (set-marker
		(make-marker)
		(org-element-property :post-affiliated elem)))
	 (end (set-marker
	       (make-marker) (org-element-property :end elem)))
	 (cont (let ((orig-elem-cont (org-dp-contents elem)))
		 (cond
		  ;; ((and (consp contents) (functionp contents))
		  ((and contents (functionp contents))
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
			(mapc
			 (lambda (--aff-kw)
			   (setq plist (plist-put
					plist --aff-kw nil)))
			 (cl-intersection plist
					  org-dp-affiliated-keys))
			plist)
		       (t plist))
		       (if (and (stringp cont)
				(memq type
				      '(item footnote-definition)))
			   (cons 'paragraph `(nil ,cont))
			 cont)
		      ;; (if (stringp cont)
		      ;; 	  (cons 'section `(nil ,cont))
		      ;; 	cont)
		      )))
    (if (and (marker-position beg)
	     (marker-position end))
	(cl-case replace
	  (append (save-excursion (goto-char end) (insert strg)))
	  (prepend (goto-char beg) (insert strg))
	  (t (if (not replace)
		 strg
	       (delete-region beg end)
	       (goto-char end)
	       (set-marker beg nil)
	       (set-marker paff nil)
	       (set-marker end nil)
	       (save-excursion (insert strg)))))
      (if replace (insert strg) strg))))


(defun org-dp-map (fun-with-args rgxp &optional match-pos backward-search-p beg end silent-p)
  "Apply quoted FUN-WITH-ARGS at every RGXP match.

If MATCH-POS is given, act conditional on its value:

 - non-nil :: (any) move point to either match-beginning
              (match-beginning 0), when forward-search is used, or
              match-end (match-end 0), when backward-search is used.

 - (sym . n) :: (cons pair) move point to sym (beg or end) of nth
                  subexpression: -> (match-beginning n)
                  or (match-end n)

Otherwise match position is not changed, so search function
`re-search-forward' will \"Set point to the end of the occurrence
found, and return point\", which is equivalent to moving point
to (match-end 0). If BACKWARD-SEARCH-P is non-nil,
`re-search-backward' is used instead, that will \"Set point to
the beginning of the match, and return point.\".

Integers BEG and/or END limit the search, if given. If SILENT-P
is non-nil, a final message reporting the total number of
mappings will be suppressed.

Given the following example Org-mode buffer

#+BEGIN_ORG
 * ORG SCRATCH
 ** Foo
 ** Bar
 ** Loo
#+END_ORG

an example call of `org-dp-map' yields significantly different
results when called with forward-search or with
backward-search. Assume FUN-WITH-ARGS is:
 
#+BEGIN_SRC emacs-lisp
  (org-dp-rewire nil (lambda (old elem) old) t nil nil
                 :tags '(\"mytag\")
                 :title (lambda (old elem) old)
                 :level 3)
#+END_SRC
 
and RGXP is \"^\\*+ \", then calling 

#+BEGIN_SRC emacs-lisp
 (org-dp-map FUN-WITH-ARGS RGXP t)
#+END_SRC

i.e. mapping with forward-search, yields

#+BEGIN_ORG
 *** ORG SCRATCH :mytag:
 ** Foo
 ** Bar
 ** Loo
#+END_ORG

while calling 
 
#+BEGIN_SRC emacs-lisp
 (org-dp-map FUN-WITH-ARGS RGXP nil t)
#+END_SRC

i.e. mapping with backward-search, yields

#+BEGIN_ORG
 *** ORG SCRATCH :mytag:
 *** Foo :mytag:
 *** Bar :mytag:
 *** Loo :mytag:
#+END_ORG

In contrast to other mapping functions in Org-mode, this mapping
function does not collect any information about mapped elements,
it simply moves point quickly to all positions in a buffer(range)
that are matched by a (forward) regexp-search and applies one of
`org-dp''s or `org-dp-lib''s functions locally at that
point (i.e. without any context information other than that about
the parsed element-at-point).

When calling FUN `org-dp-create', or `org-dp-rewire' with
argument ELEMENT given, no parsing at all takes places, but newly
created of modified elements can be inserted at point.

This mapping function wraps its body in `save-excursion' and
`save-match-data' calls, so point position and global match-data
are preserved. It does not widen the buffer before executing its
body, so buffer restrictions are respected. "
  (and (consp fun-with-args)
       (functionp (car fun-with-args))
       (org-string-nw-p rgxp)
       (let ((pt-min (or beg (point-min)))
	     (pt-max (make-marker))
	     (match-point-marker (make-marker))
	     (loop-counter 0)
	     eval-positions)
	 (unless backward-search-p
	   (set-marker-insertion-type match-point-marker t))
	 (set-marker-insertion-type pt-max t)
	 (move-marker pt-max (or end (point-max)))
	 (save-excursion
	   (save-match-data
	     (if backward-search-p
		 (goto-char pt-max)	 
	       (goto-char pt-min))
	     (while (if backward-search-p
			(re-search-backward rgxp pt-min 'NOERROR)
		      (re-search-forward rgxp pt-max 'NOERROR))
	       (move-marker match-point-marker (point))
	       (setq loop-counter (1+ loop-counter))
	       (cond
		((and match-pos (not (consp match-pos)))
		 (goto-char (if backward-search-p
				(match-end 0)
			      (match-beginning 0))))
		((and (consp match-pos)
		      (memq (car match-pos) '(beg end))
		      (integer-or-marker-p (cdr match-pos))
		      (not (eq (cdr match-pos) 0)))
		 (if (eq (car match-pos) 'beg)
		     (goto-char (match-beginning (cdr match-pos)))
		   (goto-char (match-end (cdr match-pos)))))
		(t nil))
	       (setq eval-positions (cons (point) eval-positions))
	       (eval fun-with-args)
	       (goto-char match-point-marker))))
	 (move-marker match-point-marker nil)
	 (move-marker pt-max nil)
	 (unless silent-p
	   (message
	    (concat
	    "%s\nwas called %d times at buffer positions %s "
	    "of original buffer.")
	    fun-with-args loop-counter (reverse eval-positions))))))

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
	 (beg (org-element-property :begin elem))
	 (end (org-element-property :end elem))
	 (type (org-element-type elem)))
    (if (and beg end)
	(save-restriction
	  (narrow-to-region beg end)
	  (let ((cont (org-element-map
			  (org-element-parse-buffer 'object)
			  type 'org-element-contents nil t)))
	    (cond
	     ((and interpret-p no-properties-p)
	      (org-no-properties (org-element-interpret-data cont)))
	     (interpret-p
	      (org-element-interpret-data cont))
	     (t cont))))
      (org-element-contents elem))))


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
   (list (completing-read
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
      (let* ((key (completing-read
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
		     (let ((arg (completing-read
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

(cl-defun org-dp-prompt-all (&optional elem elem-lst &key noprompt-cont noprompt-val noprompt-replace noprompt-affiliated noprompt-src-block noprompt-args)
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
		      (when (org-string-nw-p noprompt-replace)
			(intern noprompt-replace))
		    (intern
		     (org-completing-read
		     "Replace? "
		     (mapcar 'symbol-name
			     '(nil t append prepend))))))
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
			  (cl-intersection org-dp-multiple-keys
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
	   (cl-delete-duplicates
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

(cl-defun org-dp-prompt (&optional elem elem-lst partial-results-p
				 &key cont val repl aff src args)
  "Prompt user for specific properties.

This function uses `org-dp-prompt-all' to do the real work, but
follows the opposite strategy: all prompt options are turned off
by default and must be explicitly activated (while
`org-dp-prompt-all' prompts for everything that is not explicitly
deactivated). Called with no argmuents, it simply prompts for an
element type. 

See docstring of `org-dp-prompt-all' for more info about
arguments ELEM and ELEM-LST. 

If PARTIAL-RESULTS-P is non-nil, delete 'nil' values from
`org-dp-prompt-all's return-list

  (elem-type contents replace affiliated args)

otherwise simply return it 'as-is'.

Optional keyword arguments CONT, VAL, REPL, AFF, SRC and ARGS, if
given, should be either `t' (to activate prompting for them) or
of an adecuate type (see docstring of `org-dp-prompt-all') that
will be used as default value without prompting."
  (let ((res (org-dp-prompt-all
	      elem elem-lst
	      :noprompt-cont (cond
			      ((and cont (not (booleanp cont)))
			       cont)
			      ((and cont (booleanp cont)) nil)
			      (t t))
	      :noprompt-val (cond
			     ((and val (not (booleanp val)))
			      val)
			     ((and val (booleanp val)) nil)
			     (t t))
	      :noprompt-replace (cond
				 ((and repl (not (booleanp repl)))
				  repl)
				 ((and repl (booleanp repl)) nil)
				 (t t))
	      :noprompt-affiliated (cond
				    ((and aff (not (booleanp aff)))
				     aff)
				    ((and aff (booleanp aff)) nil)
				    (t t))
	      :noprompt-src-block (cond
				   ((and src (not (booleanp src)))
				    src)
				   ((and src (booleanp src)) nil)
				   (t t))
	      :noprompt-args (cond
			      ((and args (not (booleanp args)))
			       args)
			      ((and args (booleanp args)) nil)
			      (t t)))))
    (if partial-results-p  (delq nil res) res)))

(defun org-dp-apply (lst &optional fun element)
  "Apply org-dp function to (full) results LST of `org-dp-prompt'.
If FUN is non-nil, it must be `memq' of variable
`org-dp-apply-funs'. See docstring of `org-dp-prompt' for
more info about argument LST and docstring of `org-dp-rewire' for
more info about argument ELEMENT."
  (let* ((fun-no-prefix (cond
			 ((and fun (booleanp fun)) 'rewire)
			 ;; for future extensions
			 ((and fun (memq fun org-dp-apply-funs))
			  fun)
			 (t 'create)))
	 (funct (intern
		 (concat "org-dp-" (format "%s" fun-no-prefix)))))
    (cond
     ((eq fun-no-prefix 'rewire)
      (apply funct
	     (or (nth 0 lst) 'headline)
	     (nth 1 lst)
	     (nth 2 lst)
	     (nth 3 lst)
	     element
	     (car-safe (nth 4 lst))))
     ((eq fun-no-prefix 'create)
      (apply funct
	     (or (nth 0 lst) 'headline)
	     (nth 1 lst)
	     (nth 2 lst)
	     (nth 3 lst)
	     (car-safe (nth 4 lst))))
     ;; for future extensions
     (t nil))))


;;; Skeletons and Templates

;;;; Custom prompt function
(defun org-dp-tempo-read ()
  "Read function for tempo template."
  (setq org-dp-tempo-elem-type
	(ido-completing-read "ELEM-TYPE: "
			     (mapcar 'symbol-name
				     org-element-all-elements))))

;;;; Create templates

  (tempo-define-template "org-dp-create-with-comments"
    '(";; Affiliated keywords: '(:kw1 val1 :kw2 val2 ...)" n>
      ";; :name \"val\"" n>
      ";; :plot \"val\"" n>
      ";; :results (\"val\") or (\"val\" \"key\"\)" n>
      ";; :header (\"val\") or (\"val1\" \"val2\")" n>
      ";; :caption ((\"val\")) or ((\"val\" \"key\"\)) or" n>
      ";;          ((\"val2\" \"key2\"\) (\"val2\" \"key2\"\))" n>
      ";; :attr_xyz (\"val\") or (\"val1\" \"val2\")" n>
      "(org-dp-create '" (org-dp-tempo-read) 
	(if (member (intern org-dp-tempo-elem-type)
		    org-dp-no-content-elems)
	    '(l " nil t ;cont ins" n>) 
	  '(l n> "\"" p "\\n\" ;cont" n>
	  "t" > " ;ins " n>))
	  "nil" > " ;aff " n>
	org-dp-tempo-create-with-comments-elem-props
       ")" )
    "crctag"
    "Insert org-dp-create template.")

  (tempo-define-template "org-dp-create"
    '("(org-dp-create '" (org-dp-tempo-read) 
	(if (member (intern org-dp-tempo-elem-type)
		    org-dp-no-content-elems)
	    '(l " nil t ;cont ins" n>) 
	  '(l n> "\"" p "\\n\" ;cont" n>
	  "t" > " ;ins " n>))
	  "nil" > " ;aff " n>
	org-dp-tempo-create-elem-props
       ")" )
    "crtag"
    "Insert org-dp-create template.")

;;;; Rewire templates

  (tempo-define-template "org-dp-rewire-lambda"
    '("(org-dp-rewire '" (org-dp-tempo-read) 
	(if (member (intern org-dp-tempo-elem-type)
		    org-dp-no-content-elems)
	    '(l " nil t ;cont ins" n>) 
	  '(l n> "\"" p "\\n\" ;cont" n>
	  "t" > " ;ins " n>))
	  "nil" > " ;aff " n>
	  "nil" > " ;elem " n>
	org-dp-tempo-rewire-elem-props
       ")" )
    "rwltag"
    "Insert org-dp-rewire template with lambdas.")

  (tempo-define-template "org-dp-rewire"
    '("(org-dp-rewire '" (org-dp-tempo-read) 
	(if (member (intern org-dp-tempo-elem-type)
		    org-dp-no-content-elems)
	    '(l " nil t ;cont ins" n>) 
	  '(l n> "\"" p "\\n\" ;cont" n>
	  "t" > " ;ins " n>))
	  "nil" > " ;aff " n>
	  "nil" > " ;elem " n>
	org-dp-tempo-create-elem-props
       ")" )
    "rwtag"
    "Insert org-dp-rewire template.")

;;; Run Hooks and Provide

(provide 'org-dp)
;;; org-dp.el ends here
