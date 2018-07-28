- [org-dp.el &#x2014; Declarative Local Programming with Org Elements](#org-dp.el-&#x2014;-declarative-local-programming-with-org-elements)
  - [MetaData](#metadata)
  - [Commentary](#commentary)
  - [Usage](#usage)
  - [Examples](#examples)
    - [Create Src-Block](#create-src-block)
    - [Transform Src-Block into Example Block](#transform-src-block-into-example-block)
    - [Transform Src-Block into Headline](#transform-src-block-into-headline)

# org-dp.el &#x2014; Declarative Local Programming with Org Elements<a id="orgheadline8"></a>

Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
Version: 1.0
URL: <https://github.com/tj64/org-dp>
Package-Requires: ((cl-lib "0.5"))

## MetaData<a id="orgheadline1"></a>

## Commentary<a id="orgheadline2"></a>

Functions for declarative local programming with Org elements. They
allow to declare what should be done and leave the low-level work,
the "how-to", to the Org parser/interpreter framework.

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

Library org-dp is meant for programming at the local level,
i.e. without any (contextual) information except those about the
parsed element at point. It is designed to make using the Org-mode
parser/interpreter framework at local level as convenient as using
it at the global level (with a complete parse-tree produced by
\`org-element-parse-buffer\` available). It takes care of the
org-element caching mechanism in that it only acts on copies of the
locally parsed elements at point, never on the original parsed (and
cached) object itself.

## Usage<a id="orgheadline3"></a>

This library introduces a few 'public' functions

-   **\`org-dp-create':** create a new Org element by building its
    internal representation

-   **\`org-dp-rewire':** modify (and maybe transform) an existing Org
    element by changing its internal representation

-   **\`org-dp-map':** map elements in a buffer and 'rewire' them

and 1 command as generic UI

-   **\`org-dp-prompt':** universal function for getting user info

The following more 'private' functions and commands are used by the
core/UI functions, but might be useful by themselves

-   **\`org-dp-contents':** get content of (local) element

-   **\`org-dp-in':** return position-info if inside element, nil
    otherwise (not yet implemented)

-   **\`org-dp-prompt-all':** workhorse function for prompting the user

-   **\`org-dp-prompt-for-src-block-props':** prompt user for src-block
    properties (adapted from ob-core.el)

Note that the src-block parameters are appended to the src-block's
headline. If you rather want them as separate #+header: lines on
top of the src-block you can use \`org-dp-toggle-headers' from
org-dp-lib.el for swapping headers and parameters.

## Examples<a id="orgheadline7"></a>

### Create Src-Block<a id="orgheadline4"></a>

    #+BEGIN_SRC emacs-lisp
      (org-dp-create 'src-block nil nil
                     '(:name "ex1" :header (":cache no" ":noweb yes"))
                     :language "picolisp"
                     :preserve-indent 1
                     :parameters ":results value"
                     :value "(+ 2 2)")
    #+END_SRC

    #+NAME: ex1
    #+HEADER: :noweb yes
    #+HEADER: :cache no
    #+BEGIN_SRC picolisp :results value
    (+ 2 2)
    #+END_SRC

### Transform Src-Block into Example Block<a id="orgheadline5"></a>

    #+NAME: ex2
    #+HEADER: :results raw
    #+BEGIN_SRC emacs-lisp  :exports both
     (org-dp-rewire 'example-block) 
    #+END_SRC

    #+results: ex2
    #+BEGIN_EXAMPLE
    (org-dp-rewire 'example-block) 
    #+END_EXAMPLE

### Transform Src-Block into Headline<a id="orgheadline6"></a>

    #+NAME: ex2
    #+HEADER: :results raw
    #+BEGIN_SRC emacs-lisp :cache no :noweb yes
    (org-dp-rewire 'headline
                   (lambda (_cont_ elem)
                     (concat
                      "This was an\n\n"
                      (org-element-property :language elem)
                      "\n\nsrc-block with header args\n\n"
                      (org-element-property :parameters elem)
                      "\n\nbefore."))
                   'append
                   '(:name "transformed Src-Block")
                   nil
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
    #+END_SRC

    #+NAME: transformed Src-Block
    *  RESULTS RAW :ex2:
    This was an
    
    emacs-lisp
    
    src-block with header args
    
    :cache no :noweb yes
    
    before.
