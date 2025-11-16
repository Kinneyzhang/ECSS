;;; combinator-examples.el --- Examples of CSS combinator usage -*- lexical-binding: t; -*-

;; This file demonstrates the fixed CSS combinator functionality in ECSS.

;;; Code:

(require 'ecss-selector)
(require 'ecss-dom)

;;; Example DOM Structure

;; The following DOM structure will be used for examples:
;;
;; <html>
;;   <body>
;;     <div id="container">
;;       <h1>Title</h1>
;;       <p class="intro">Introduction paragraph</p>
;;       <p class="content">Content paragraph</p>
;;       <span class="note">Note span</span>
;;       <div class="nested">
;;         <p>Nested paragraph</p>
;;       </div>
;;     </div>
;;   </body>
;; </html>

(defvar example-dom
  '(html nil
         (body nil
               (div ((id . "container"))
                    (h1 nil "Title")
                    (p ((class . "intro")) "Introduction paragraph")
                    (p ((class . "content")) "Content paragraph")
                    (span ((class . "note")) "Note span")
                    (div ((class . "nested"))
                         (p nil "Nested paragraph")))))
  "Example DOM structure for testing combinators.")

;;; Child Combinator (>)

;; The child combinator selects elements that are direct children of another element.

;; Example 1: Select all <p> elements that are direct children of <div#container>
;; CSS: div#container > p
;; Expected: 2 <p> elements (intro and content, but not the nested one)

(defun example-child-combinator-1 ()
  "Example of child combinator selecting direct <p> children of <div#container>."
  (let ((results (ecss-dom-query-selector-all example-dom "div#container > p")))
    (message "Query: div#container > p")
    (message "Results: %d elements" (length results))
    (dolist (node results)
      (message "  - %s with class '%s'"
               (dom-tag node)
               (dom-attr node 'class)))
    results))

;; Example 2: Select all direct children of <div#container>
;; CSS: div#container > *
;; Expected: 5 elements (h1, 2 p, span, and nested div)

(defun example-child-combinator-2 ()
  "Example of child combinator with universal selector."
  (let ((results (ecss-dom-query-selector-all example-dom "div#container > *")))
    (message "Query: div#container > *")
    (message "Results: %d elements" (length results))
    (dolist (node results)
      (message "  - %s" (dom-tag node)))
    results))

;;; Adjacent Sibling Combinator (+)

;; The adjacent sibling combinator selects an element that is immediately
;; preceded by a specific element.

;; Example 3: Select <p> that immediately follows <p.intro>
;; CSS: p.intro + p
;; Expected: 1 element (<p.content>)

(defun example-adjacent-sibling-1 ()
  "Example of adjacent sibling combinator."
  (let ((results (ecss-dom-query-selector-all example-dom "p.intro + p")))
    (message "Query: p.intro + p")
    (message "Results: %d elements" (length results))
    (dolist (node results)
      (message "  - %s with class '%s'"
               (dom-tag node)
               (dom-attr node 'class)))
    results))

;; Example 4: Select <span> that immediately follows <p>
;; CSS: p + span
;; Expected: 1 element (<span.note>)

(defun example-adjacent-sibling-2 ()
  "Example of adjacent sibling combinator selecting span after p."
  (let ((results (ecss-dom-query-selector-all example-dom "p + span")))
    (message "Query: p + span")
    (message "Results: %d elements" (length results))
    (dolist (node results)
      (message "  - %s with class '%s'"
               (dom-tag node)
               (dom-attr node 'class)))
    results))

;; Example 5: Non-matching adjacent sibling
;; CSS: h1 + span
;; Expected: 0 elements (span is not immediately after h1)

(defun example-adjacent-sibling-3 ()
  "Example showing non-matching adjacent sibling."
  (let ((results (ecss-dom-query-selector-all example-dom "h1 + span")))
    (message "Query: h1 + span")
    (message "Results: %d elements" (length results))
    results))

;;; General Sibling Combinator (~)

;; The general sibling combinator selects all elements that are siblings
;; of a specified element (and come after it in the document).

;; Example 6: Select all <p> elements that are siblings after <h1>
;; CSS: h1 ~ p
;; Expected: 2 elements (intro and content)

(defun example-general-sibling-1 ()
  "Example of general sibling combinator selecting p after h1."
  (let ((results (ecss-dom-query-selector-all example-dom "h1 ~ p")))
    (message "Query: h1 ~ p")
    (message "Results: %d elements" (length results))
    (dolist (node results)
      (message "  - %s with class '%s'"
               (dom-tag node)
               (dom-attr node 'class)))
    results))

;; Example 7: Select all elements that are siblings after <p.intro>
;; CSS: p.intro ~ *
;; Expected: 3 elements (p.content, span.note, div.nested)

(defun example-general-sibling-2 ()
  "Example of general sibling combinator with universal selector."
  (let ((results (ecss-dom-query-selector-all example-dom "p.intro ~ *")))
    (message "Query: p.intro ~ *")
    (message "Results: %d elements" (length results))
    (dolist (node results)
      (message "  - %s" (dom-tag node)))
    results))

;; Example 8: Select <span> that is a sibling after <p>
;; CSS: p ~ span
;; Expected: 1 element (<span.note>)

(defun example-general-sibling-3 ()
  "Example of general sibling combinator selecting span after any p."
  (let ((results (ecss-dom-query-selector-all example-dom "p ~ span")))
    (message "Query: p ~ span")
    (message "Results: %d elements" (length results))
    (dolist (node results)
      (message "  - %s with class '%s'"
               (dom-tag node)
               (dom-attr node 'class)))
    results))

;;; Descendant Combinator (space)

;; The descendant combinator selects all elements that are descendants
;; of a specified element (not just direct children).

;; Example 9: Select all <p> elements inside <div#container>
;; CSS: div#container p
;; Expected: 3 elements (intro, content, and nested)

(defun example-descendant-combinator ()
  "Example of descendant combinator."
  (let ((results (ecss-dom-query-selector-all example-dom "div#container p")))
    (message "Query: div#container p")
    (message "Results: %d elements" (length results))
    (dolist (node results)
      (message "  - %s with class '%s'"
               (dom-tag node)
               (or (dom-attr node 'class) "none")))
    results))

;;; Running All Examples

(defun run-all-combinator-examples ()
  "Run all combinator examples and display results."
  (interactive)
  (message "\n=== CSS Combinator Examples ===\n")
  
  (message "\n--- Child Combinator (>) ---")
  (example-child-combinator-1)
  (message "")
  (example-child-combinator-2)
  
  (message "\n--- Adjacent Sibling Combinator (+) ---")
  (example-adjacent-sibling-1)
  (message "")
  (example-adjacent-sibling-2)
  (message "")
  (example-adjacent-sibling-3)
  
  (message "\n--- General Sibling Combinator (~) ---")
  (example-general-sibling-1)
  (message "")
  (example-general-sibling-2)
  (message "")
  (example-general-sibling-3)
  
  (message "\n--- Descendant Combinator (space) ---")
  (example-descendant-combinator)
  
  (message "\n=== All Examples Complete ==="))

(provide 'combinator-examples)

;;; combinator-examples.el ends here
