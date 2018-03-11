;;; org-dp-lib.el --- Library of org-dp functions
;; Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
;; Version: 1.0
;; URL: https://github.com/tj64/org-dp

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
;; Library of (hopefully) useful functions written with org-dp.el.

;;; Requires

(require 'org-dp)


;;; Custom Vars

(defcustom org-dp-misc-props (list "ID")
  "Other Org properties."
  :group 'org-dp
  :type '(repeat string))

;;; Utilities
;;; Applications
;;;; Wrap in Block

(defun org-dp-wrap-in-block (&optional lines user-info &rest prompt-spec)
  "Wrap sexp-at-point or region in Org block.

A region instead of the sexp-at-point is wrapped if either

   - optional arg LINES is an (positive or negative) integer or

   - the region is active

In the first case the region is determined by moving LINES lines
up (LINES is positive) or down (LINES is negative) from point
using `forward-line', in the second case the active region is
used.

If point is already inside of a block, modify it or unwrap its
content/value instead of wrapping it in another block, except if
explicitly asked for by user.

If USER-INFO is given, it should be a list in the format returned
by `org-dp-prompt-all', i.e.

 (elem-type contents replace affiliated args)

Look up that function's docstring for more information about the
list's elements. A non-nil USER-INFO suppresses calls to
`org-dp-prompt-all' and is used instead of its return value.

Possible &rest PROMPT-SPEC should be keyword/value pairs used for
restricting user-prompting via `org-dp-prompt-all', e.g.

  :noprompt-affiliated t :noprompt-replace t

see the docstring of that function for more info."
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
			  (cl-case type
			    ((center-block
			      quote-block special-block)
			     (org-dp-contents elem t))
			    ((comment-block example-block)
			     (org-element-property :value elem))
			    (t nil))))
		      t))))
		 (user-error "Block content unwrapped."))
		;; replace surrounding block
		((y-or-n-p "Replace surrounding block ")
		 (save-excursion
		   (goto-char (or (car dblock-limits)
				  src-block-beg
				  (car block-limits)))
		   (let* ((elem-at-pt (cl-copy-list
				       (org-element-at-point))) 
			  (usrinfo
			   ;; ;; FIXME user-info not yet defined here
			   ;; (if (and user-info (consp user-info))
			   ;;     user-info
			   (org-dp-prompt-all
			    elem-at-pt			   
			    (list 'src-block 'dynamic-block
				  'center-block 'quote-block
				  'special-block 'comment-block
				  'example-block)))) ;)
		     (apply 'org-dp-rewire
			    (nth 0 usrinfo)
			    (nth 1 usrinfo)
			    (nth 2 usrinfo)
			    (nth 3 usrinfo)
			    elem-at-pt
			    (nth 4 usrinfo))))
		 (user-error "Surrounding block replaced."))
		;; nest blocks or abort
		(t (if (y-or-n-p "Really nest blocks ")
		       nil
		     (user-error "Action aborted."))))))))
     (when current-prefix-arg 
       (list (read-number "Number of lines to wrap: " 1)))))
  (let* ((usrinfo (or (when (consp user-info) user-info)
		      (apply 'org-dp-prompt-all
		       nil
		       '(src-block dynamic-block center-block
				   quote-block special-block
				   comment-block
				   example-block verse-block)
		       (progn
			 (setq prompt-spec
			       (plist-put prompt-spec
					  :noprompt-cont t))
			 (setq prompt-spec
			       (plist-put prompt-spec
					  :noprompt-val t))
			 (setq prompt-spec
			       (plist-put prompt-spec
					  :noprompt-replace t))
			 prompt-spec))))
	 (empty-line (save-match-data
		       (looking-at "^[[:space:]]*$")))
	 (beg (or (and (not lines)
		       (region-active-p)
		       (region-beginning))
		  (point)))
	 (marker (save-excursion
		   (goto-char beg) (point-marker)))
	 (bol (save-excursion (goto-char beg) (bolp)))
	 (end (cond
	       (lines (save-excursion
			(forward-line lines) (point)))
	       ((region-active-p)(region-end))
	       (empty-line (save-excursion
			     (forward-line) (point)))
	       (t (save-excursion
		    (forward-sexp) (point)))))
	 (cut-strg (buffer-substring beg end)))
    (delete-region beg end)
    (goto-char (marker-position marker))
    (apply 'org-dp-create
	   (nth 0 usrinfo)
	   (or (org-string-nw-p cut-strg) "\n")
	   t
	   (nth 3 usrinfo)
	   (nth 4 usrinfo))
    (set-marker marker nil)
    (goto-char (save-excursion
		 (goto-char (re-search-backward
			     "^#\\+[^\000]+?\n#\\+"
			     nil 'NOERROR 1))
		 (forward-line)
		 (point)))) )

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
    (cl-case act
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
		    (cl-remove-duplicates (append headers _old_))))))
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
		       (if (org-string-nw-p _old_)
			   (concat " " _old_)
			 "")))
	:header nil))
      (t (error "Not a valid action: %s" act)))))

;;;; Filter Node Properties

(defconst org-dp-prop-classes
  '((special . org-special-properties)
    (custom . org-custom-properties)
    (default . org-default-properties)
    (file . org-file-properties)
    (global . (mapcar 'car org-global-properties)))
  "Alist of filter-types and associated property-classes.")

(defun org-dp-org-props ()
  "Return all Org (i.e. system) properties.
Calculate the union of special, custom, default, file and global
properties, add the properties specified by the user in
customizable variable `org-dp-misc-props', and return result as
list of strings without duplicates."
  (let (org-props)
      (mapc
       (lambda (--prop-class)
	 (setq org-props
	       (append
		(eval (cdr-safe --prop-class)) org-props)))
       org-dp-prop-classes)
    (ignore-errors
      (cl-delete-duplicates
       (append org-dp-misc-props org-props)))))

(defun org-dp-filter-node-props (filter &optional negate-p verbose-p)
  "Return filtered node-properties.
FILTER should be either a symbol from `org-dp-prop-classes', the
symbol `org' (matching the union of all `org-dp-prop-classes' and
customizable variable `org-dp-misc-props'), a list of keys as
strings, or a (single) regexp-string. If NEGATE-P is non-nil, the
properties not matched by the filter are returned. If VERBOSE-P
is non-nil, a message is printed if no property-drawer is found,
otherwise nil is returned."
  (let* ((elem (save-excursion
		 (and
		  (or (org-at-heading-p)
		      (outline-previous-heading))
		  (org-element-at-point))))
	 (beg (org-element-property :begin elem))
	 (end (org-element-property :end elem))
	 (props (progn
		  (and beg end)
		  (save-restriction
		    (narrow-to-region beg end)
		    (org-element-map
			(org-element-parse-buffer 'object)
			'property-drawer
		      'org-element-contents nil t) )))
	 filtered-props)
    (if (not props)
	(when verbose-p
	  (message
	   "Could not find properties at point %d in buffer %s."
	   (point) (current-buffer)))
      (org-element-map props 'node-property
	(lambda (--prop)
	  (let* ((key (org-element-property :key --prop))
		 (val (org-element-property :value --prop))
		 (memberp (cl-case filter
			    ((special custom default file global)
			     (member-ignore-case
			      key
			      (eval
			       (cdr-safe
				(assoc filter
				       org-dp-prop-classes)))))
			    (org (member-ignore-case
				  key (org-dp-org-props)))
			    (t (cond
				((stringp filter)
				 (string-match filter key))
				((consp filter)
				 (member-ignore-case key filter))
				(t (error "Not a valid filter: %s"
					  filter)))))))
	    (when (or (and negate-p (not memberp))
		      (and (not negate-p) memberp))
	      (setq filtered-props
		    (cons (cons key val) filtered-props))))))
      filtered-props)))


	 ;; 	  (re-search-forward
	 ;; 	   org-property-drawer-re
	 ;; 	   (save-excursion
	 ;; 	     (org-end-of-meta-data t)
	 ;; 	     (point))
	 ;; 	   'NOERROR 1)
	 ;; 	  (progn
	 ;; 	    (goto-char (match-beginning 0))
	 ;; 	    (org-dp-contents)))))
	 ;; (props (save-excursion
	 ;; 	 (and
	 ;; 	  (or (org-at-heading-p)
	 ;; 	      (outline-previous-heading))
	 ;; 	  (org-element-map
	 ;; 		  (org-element-parse-buffer 'object)
	 ;; 		  type 'org-element-contents nil t)
		  

	 ;; 	  (re-search-forward
	 ;; 	   org-property-drawer-re
	 ;; 	   (save-excursion
	 ;; 	     (org-end-of-meta-data t)
	 ;; 	     (point))
	 ;; 	   'NOERROR 1)
	 ;; 	  (progn
	 ;; 	    (goto-char (match-beginning 0))
	 ;; 	    (org-dp-contents)))))


;;;; Create Composite Org Elements

;;;;; Table

(defun org-dp-create-table (row-lst &optional tblfm table-el-p insert-p)
  "Create table with content ROW-LST.
ROW-LST is an alist of lists with elements that are contents of a
single row's table cells, e.g.

 (list
   (list \"col1\" \"col2\" \"col3\")
   'hline
   (list 1 2 3)
   (list 'x 'y 'z))

for a table with 3 columns and 4 rows, including one horizontal
line.

TBLFM, if given, should be a list containing a table formula as
string.

If TABLE-EL-P is non-nil, the table type will be 'table.el',
otherwise it is 'org' by default.

If INSERT-P is non-nil, insert table at point, otherwise return
it as string."
  (let ((cont (mapconcat
	       (lambda (--row-cells)
		 (if (eq --row-cells 'hline)
		     ;; rule rows
		     (org-dp-create 'table-row nil nil nil
				    :type 'rule)
		   ;; standard rows
		   (org-dp-create 'table-row
				  (mapconcat
				   (lambda (--cell-cont)
				     ;; cells
				     (org-dp-create
				      'table-cell
				      (format "%s" --cell-cont)))
				   --row-cells "")
				  nil nil :type 'standard)))
	       row-lst "")))
    (org-dp-create 'table
		   (unless table-el-p cont)
		   insert-p
		   nil
		   :type (if table-el-p 'table.el 'org)
		   :tblfm tblfm
		   :value (when table-el-p cont))))

;;;;; Plain List


(defun org-dp-create-plain-list (item-lst &optional insert-p)
  "Create plain list with content ITEM-LST.

ITEM-LST is an alist of list items with a content-string in the
car and the item's property values in the cdr (in the
order :bullet, :tag, :checkbox and :counter), e.g.:

 (list
   '(\"world\" \"-\" \"hello\" on 2)
   '(\"up\" \"1\" \"whats\" 'off)
   '(\"there?\" \"a\" \"out\" 'trans)
   '(\"news?\" \"B\"))

If INSERT-P is non-nil, insert plain list at point, otherwise
return it as string."
  (let ((cont (mapconcat
	       (lambda (--item)
		 (let* ((props (cdr --item))
			(bull (or (nth 0 props) "-"))
			(tag (nth 1 props))
			(cbox (nth 2 props)))
		   (org-dp-create 'item (car --item) nil nil
				  :bullet (when bull
					    (format "%s" bull))
				  :tag  (when tag
					  (format "%s" tag))
				  :checkbox (when cbox
					      (format "%s" cbox))
				  :counter (nth 3 props))))
		   item-lst "")))
    (org-dp-create 'plain-list cont insert-p)))



;;;;; Node Properties

(defun org-dp-create-property-drawer (node-prop-lst &optional insert-p)
  "Create property drawer with content NODE-PROP-LST.

NODE-PROP-LST is an alist of lists with two elements, the first
being the :key, the second the :value, e.g.:

 (list '(\"key1\" \"val1\") '(\"key2\" \"val2\") or
 '((key1 val1) (key2 val2))

The elements of NODE-PROP-LST are formatted before use, so they
can be of any type (string, symbol, number etc.).

If INSERT-P is non-nil, insert property drawer at point,
otherwise return it as string."
  (let ((cont (mapconcat
	       (lambda (--node-prop)
		 (org-dp-create 'node-property nil nil nil
				:key (format
				      "%s" (car --node-prop))
				:value (format
					"%s" (cadr --node-prop))))
	       node-prop-lst "")))
    (org-dp-create 'property-drawer cont insert-p)))


;;; Run Hooks and Provide

(provide 'org-dp-lib)

;;; org-dp-lib.el ends here

