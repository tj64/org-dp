- [org-dp.el &#x2014; Declarative Programming with Org Elements](#org-dp.el-&#x2014;-declarative-programming-with-org-elements)
  - [MetaData](#metadata)
  - [Commentary](#commentary)
  - [Usage](#usage)
  - [Examples](#examples)
    - [Create Src-Block](#create-src-block)
    - [Transform Src-Block into Example Block](#transform-src-block-into-example-block)
- [:RESULTS RAW](#:results-raw)
- [:RESULTS RAW](#:results-raw)
- [:RESULTS RAW](#:results-raw)
    - [Transform Src-Block into Headline](#transform-src-block-into-headline)
- [RESULTS RAW](#results-raw)
- [:RESULTS PP](#:results-pp)



# org-dp.el &#x2014; Declarative Programming with Org Elements<a id="sec-1"></a>

Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
Version: 0.9
URL: <https://github.com/tj64/org-dp>

## MetaData<a id="sec-1-1"></a>

    copyright: Thorsten Jolitz
    
    copyright-years: 2014+
    
    version: 0.9
    
    licence: GPL 3 or later (free software)
    
    licence-url: http://www.gnu.org/licenses/
    
    part-of-emacs: no
    
    author: Thorsten Jolitz
    
    author_email: tjolitz AT gmail DOT com
    
    keywords: emacs org-mode org-elements declarative-programming
    
    git-repo: https://github.com/tj64/org-dp
    
    git-clone: git://github.com/tj64/org-dp.git

## Commentary<a id="sec-1-2"></a>

Functions for declarative programming with Org elements. They allow
to declare what should be done and leave the low-level work, the
"how-to", to the Org parser/interpreter framework.

With other words, org-dp acts on the internal representation of Org
elements rather than on their textual representation, and leaves
the transformation between both representations to the
parser/interpreter framework. To create or modify an element, you
call the parser to open it up, rewire its internals, and then call
the interpreter to build the element again based on its modified
internals.

Since all Org elements are uniformely represented as nested lists
internally, with their properties stored as key-val pairs in
plists, they can be treated in a much more uniform way when dealing
with the internal representation instead of the highly variable
textual representations. A big advantage of plists is that only
those properties that are actually accessed matter, so when
transforming one Org element into another on the internal level one
does not have to worry about not matching properties as long as
these are not used by the interpreter when building the textual
representation of the transformed element.

## Usage<a id="sec-1-3"></a>

This library introduces a few 'public' functions

-   **\`org-dp-create':** create a new Org element by building its
    internal representation

-   **\`org-dp-rewire':** modify (and maybe transform) and existing Org
    element by changing its internal representation

-   **\`org-dp-map':** map elements in a buffer and 'rewire' them (not
    yet implemented)

and 1 command as generic UI

-   **\`org-dp-prompt':** universal function for getting user info

The following more 'private' functions and commands are used by the
core/UI functions, but might be useful by themselves

-   **\`org-dp-contents':** get content of (local) element

-   **\`org-dp-in':** return position-info if inside element, nil
    otherwise (not yet implemented)

-   **\`org-dp-prompt-for-src-block-props':** prompt user for src-block
    properties (adapted from ob-core.el)

Note that the src-block parameters are appended to the src-block's
headline. If you rather want them as separate #+header: lines on
top of the src-block you can use \`org-dp-toggle-headers' from
org-dp-lib.el for swapping headers and parameters.

## Examples<a id="sec-1-4"></a>

### Create Src-Block<a id="sec-1-4-1"></a>

```lisp
(org-dp-create 'src-block nil nil
	       '(:name "ex1" :header (":cache no" ":noweb yes"))
	       :language "picolisp"
	       :preserve-indent 1
	       :parameters ":results value"
	       :value "(+ 2 2)")
```

### Transform Src-Block into Example Block<a id="sec-1-4-2"></a>

```lisp
(org-dp-rewire 'example-block)
```

nil

# :RESULTS RAW     :ex2:<a id="sec-2"></a>

This was an

emacs-lisp

src-block with header args

before.

# :RESULTS RAW     :ex2:<a id="sec-3"></a>

This was an

emacs-lisp

src-block with header args

before.

# :RESULTS RAW     :ex2:<a id="sec-4"></a>

This was an

emacs-lisp

src-block with header args

:exports both

before.

### Transform Src-Block into Headline<a id="sec-4-0-1"></a>

```lisp
(org-dp-rewire 'headline
	       (lambda (_cont_ elem)
		 (concat
		  "This was an\n\n"
		  (org-element-property :language elem)
		  "\n\nsrc-block with header args\n\n"
		  (org-element-property :parameters elem)
		  "\n\nbefore."))
		  'append '(:name "transformed Src-Block")
		  :level 1
		  :title (lambda (_old_ elem)
			   (mapconcat
			    'upcase
			    (split-string
			     (car
			      (org-element-property :header elem))
			      ":")
			    " "))
		  :tags (lambda (_old_ elem)
			  (list (org-element-property :name elem)))
		  :header nil)
```

# RESULTS RAW     :ex2:<a id="sec-5"></a>

This was an

emacs-lisp

src-block with header args

before.

# :RESULTS PP     :ex2:<a id="sec-6"></a>

This was an

emacs-lisp

src-block with header args

before.
