;;; test-pseudo-classes.el --- Tests for pseudo-class selectors -*- lexical-binding: t; -*-

;; This file contains comprehensive tests for pseudo-class selector matching in ecss-dom.el

;;; Code:

(require 'ecss-selector)
(require 'ecss-dom)

;;; Test Helper Functions

(defun test-create-sample-dom ()
  "Create a sample DOM tree for testing."
  '(html nil
         (head nil
               (title nil "Test Page"))
         (body nil
               (div ((id . "container"))
                    (h1 nil "Title")
                    (p ((class . "intro")) "First paragraph")
                    (p nil "Second paragraph")
                    (p nil "Third paragraph")
                    (p ((class . "outro")) "Fourth paragraph"))
               (div ((class . "content"))
                    (article nil
                             (h2 nil "Article Title")
                             (p nil "Article content"))
                    (article nil
                             (h2 nil "Second Article")
                             (p nil "More content"))
                    (article ((class . "featured")) 
                             (h2 nil "Featured")
                             (p nil "Featured content")))
               (ul nil
                   (li nil "Item 1")
                   (li nil "Item 2")
                   (li nil "Item 3")
                   (li nil "Item 4")
                   (li nil "Item 5"))
               (div ((class . "empty-tests"))
                    (div ((class . "empty")) "   ")
                    (div ((class . "not-empty")) "Content")
                    (div ((class . "truly-empty")))))))

;;; Test Functions

(defun test-first-child ()
  "Test :first-child pseudo-class."
  (message "\n=== Testing :first-child ===\n")
  
  ;; h1 is the first child in the #container div
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "h1:first-child")))
    (message "Testing h1:first-child")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ h1:first-child works"))
  
  ;; title is the first (and only) child in head
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "title:first-child")))
    (message "Testing title:first-child")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ title:first-child works"))
  
  ;; article is the first child in .content div
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "article:first-child")))
    (message "Testing article:first-child")
    (message "Found %d nodes" (length results))
    (cl-assert (>= (length results) 1))
    (message "✓ article:first-child works"))
  
  (message "\n✓ All :first-child tests passed!\n"))

(defun test-last-child ()
  "Test :last-child pseudo-class."
  (message "\n=== Testing :last-child ===\n")
  
  ;; The last p in #container div has class outro
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom ".outro:last-child")))
    (message "Testing .outro:last-child")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ .outro:last-child works"))
  
  ;; Test last li
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "li:last-child")))
    (message "Testing li:last-child")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ li:last-child works"))
  
  ;; Test last article (has class featured)
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom ".featured:last-child")))
    (message "Testing .featured:last-child")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ .featured:last-child works"))
  
  (message "\n✓ All :last-child tests passed!\n"))

(defun test-only-child ()
  "Test :only-child pseudo-class."
  (message "\n=== Testing :only-child ===\n")
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "title:only-child")))
    (message "Testing title:only-child")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ :only-child works"))
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "h1:only-child")))
    (message "Testing h1:only-child (should not match)")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 0))
    (message "✓ :only-child correctly rejects non-only children"))
  
  (message "\n✓ All :only-child tests passed!\n"))

(defun test-first-of-type ()
  "Test :first-of-type pseudo-class."
  (message "\n=== Testing :first-of-type ===\n")
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "p:first-of-type")))
    (message "Testing p:first-of-type")
    (message "Found %d nodes" (length results))
    ;; Multiple divs, each can have a first p
    (cl-assert (> (length results) 0))
    (message "✓ :first-of-type works"))
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "article:first-of-type")))
    (message "Testing article:first-of-type")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ article:first-of-type works"))
  
  (message "\n✓ All :first-of-type tests passed!\n"))

(defun test-last-of-type ()
  "Test :last-of-type pseudo-class."
  (message "\n=== Testing :last-of-type ===\n")
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "p:last-of-type")))
    (message "Testing p:last-of-type")
    (message "Found %d nodes" (length results))
    (cl-assert (> (length results) 0))
    (message "✓ :last-of-type works"))
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "article:last-of-type")))
    (message "Testing article:last-of-type")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ article:last-of-type works"))
  
  (message "\n✓ All :last-of-type tests passed!\n"))

(defun test-only-of-type ()
  "Test :only-of-type pseudo-class."
  (message "\n=== Testing :only-of-type ===\n")
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "title:only-of-type")))
    (message "Testing title:only-of-type")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ :only-of-type works"))
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "ul:only-of-type")))
    (message "Testing ul:only-of-type")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ ul:only-of-type works"))
  
  (message "\n✓ All :only-of-type tests passed!\n"))

(defun test-empty ()
  "Test :empty pseudo-class."
  (message "\n=== Testing :empty ===\n")
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom ".truly-empty:empty")))
    (message "Testing .truly-empty:empty")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ :empty works for truly empty elements"))
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom ".empty:empty")))
    (message "Testing .empty:empty (whitespace only)")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ :empty works for whitespace-only elements"))
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom ".not-empty:empty")))
    (message "Testing .not-empty:empty (should not match)")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 0))
    (message "✓ :empty correctly rejects non-empty elements"))
  
  (message "\n✓ All :empty tests passed!\n"))

(defun test-nth-child ()
  "Test :nth-child() pseudo-class."
  (message "\n=== Testing :nth-child() ===\n")
  
  ;; Test specific index
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "li:nth-child(2)")))
    (message "Testing li:nth-child(2)")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ :nth-child(n) works"))
  
  ;; Test odd
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "li:nth-child(odd)")))
    (message "Testing li:nth-child(odd)")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 3))
    (message "✓ :nth-child(odd) works"))
  
  ;; Test even
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "li:nth-child(even)")))
    (message "Testing li:nth-child(even)")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 2))
    (message "✓ :nth-child(even) works"))
  
  ;; Test 2n+1
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "li:nth-child(2n+1)")))
    (message "Testing li:nth-child(2n+1)")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 3))
    (message "✓ :nth-child(2n+1) works"))
  
  (message "\n✓ All :nth-child() tests passed!\n"))

(defun test-nth-last-child ()
  "Test :nth-last-child() pseudo-class."
  (message "\n=== Testing :nth-last-child() ===\n")
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "li:nth-last-child(1)")))
    (message "Testing li:nth-last-child(1)")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ :nth-last-child(1) works"))
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "li:nth-last-child(2)")))
    (message "Testing li:nth-last-child(2)")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ :nth-last-child(2) works"))
  
  (message "\n✓ All :nth-last-child() tests passed!\n"))

(defun test-nth-of-type ()
  "Test :nth-of-type() pseudo-class."
  (message "\n=== Testing :nth-of-type() ===\n")
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "article:nth-of-type(2)")))
    (message "Testing article:nth-of-type(2)")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ :nth-of-type(2) works"))
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "p:nth-of-type(1)")))
    (message "Testing p:nth-of-type(1)")
    (message "Found %d nodes" (length results))
    (cl-assert (> (length results) 0))
    (message "✓ :nth-of-type(1) works"))
  
  (message "\n✓ All :nth-of-type() tests passed!\n"))

(defun test-nth-last-of-type ()
  "Test :nth-last-of-type() pseudo-class."
  (message "\n=== Testing :nth-last-of-type() ===\n")
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "article:nth-last-of-type(1)")))
    (message "Testing article:nth-last-of-type(1)")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ :nth-last-of-type(1) works"))
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "p:nth-last-of-type(1)")))
    (message "Testing p:nth-last-of-type(1)")
    (message "Found %d nodes" (length results))
    (cl-assert (> (length results) 0))
    (message "✓ :nth-last-of-type(1) works"))
  
  (message "\n✓ All :nth-last-of-type() tests passed!\n"))

(defun test-not-pseudo-class ()
  "Test :not() pseudo-class."
  (message "\n=== Testing :not() ===\n")
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "p:not(.intro)")))
    (message "Testing p:not(.intro)")
    (message "Found %d nodes" (length results))
    ;; Should find all p elements except the one with class="intro"
    (cl-assert (> (length results) 0))
    (dolist (node results)
      (cl-assert (not (ecss-dom-has-class node "intro"))))
    (message "✓ :not(.class) works"))
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "article:not(.featured)")))
    (message "Testing article:not(.featured)")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 2))
    (message "✓ :not(.featured) works"))
  
  (message "\n✓ All :not() tests passed!\n"))

(defun test-combined-pseudo-classes ()
  "Test combinations of pseudo-classes."
  (message "\n=== Testing Combined Pseudo-classes ===\n")
  
  (let* ((dom (test-create-sample-dom))
         (results (ecss-dom-query-selector-all dom "h1:first-child:not(.intro)")))
    (message "Testing h1:first-child:not(.intro)")
    (message "Found %d nodes" (length results))
    (cl-assert (= (length results) 1))
    (message "✓ Combined pseudo-classes work"))
  
  (message "\n✓ All combined pseudo-class tests passed!\n"))

(defun test-nth-expression-parsing ()
  "Test nth expression parsing."
  (message "\n=== Testing Nth Expression Parsing ===\n")
  
  (let ((result (ecss-dom-parse-nth-expression "2n+1")))
    (cl-assert (equal result '(2 . 1)))
    (message "✓ Parsed '2n+1' correctly"))
  
  (let ((result (ecss-dom-parse-nth-expression "odd")))
    (cl-assert (equal result '(2 . 1)))
    (message "✓ Parsed 'odd' correctly"))
  
  (let ((result (ecss-dom-parse-nth-expression "even")))
    (cl-assert (equal result '(2 . 0)))
    (message "✓ Parsed 'even' correctly"))
  
  (let ((result (ecss-dom-parse-nth-expression "3")))
    (cl-assert (equal result '(0 . 3)))
    (message "✓ Parsed '3' correctly"))
  
  (let ((result (ecss-dom-parse-nth-expression "n")))
    (cl-assert (equal result '(1 . 0)))
    (message "✓ Parsed 'n' correctly"))
  
  (let ((result (ecss-dom-parse-nth-expression "-n+6")))
    (cl-assert (equal result '(-1 . 6)))
    (message "✓ Parsed '-n+6' correctly"))
  
  (message "\n✓ All nth expression parsing tests passed!\n"))

;;; Main Test Runner

(defun run-all-pseudo-class-tests ()
  "Run all pseudo-class tests."
  (interactive)
  (message "\n========================================")
  (message "Running Pseudo-class Tests")
  (message "========================================\n")
  
  (test-nth-expression-parsing)
  (test-first-child)
  (test-last-child)
  (test-only-child)
  (test-first-of-type)
  (test-last-of-type)
  (test-only-of-type)
  (test-empty)
  (test-nth-child)
  (test-nth-last-child)
  (test-nth-of-type)
  (test-nth-last-of-type)
  (test-not-pseudo-class)
  (test-combined-pseudo-classes)
  
  (message "\n========================================")
  (message "✓✓✓ ALL PSEUDO-CLASS TESTS PASSED! ✓✓✓")
  (message "========================================\n"))

(provide 'test-pseudo-classes)

;;; test-pseudo-classes.el ends here
