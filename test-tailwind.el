;;; test-tailwind.el --- Tests for Tailwind CSS support -*- lexical-binding: t; -*-

;; This file contains tests and examples for the Tailwind CSS support in ECSS

;;; Code:

(require 'ecss-tailwind)

;;; Test cases for Tailwind class parsing

(defun test-tailwind-parse ()
  "Test Tailwind class parsing."
  (message "\n=== Testing Tailwind Class Parsing ===\n")
  
  ;; Test simple utility class
  (let ((result (ecss-tailwind-parse-class "flex")))
    (message "Parse 'flex': %S" result)
    (cl-assert (equal (plist-get result :utility) "flex"))
    (cl-assert (equal (plist-get result :property) "flex")))
  
  ;; Test utility with value
  (let ((result (ecss-tailwind-parse-class "bg-red-500")))
    (message "Parse 'bg-red-500': %S" result)
    (cl-assert (equal (plist-get result :utility) "bg-red-500"))
    (cl-assert (equal (plist-get result :property) "bg"))
    (cl-assert (equal (plist-get result :value) "red-500")))
  
  ;; Test with responsive prefix
  (let ((result (ecss-tailwind-parse-class "md:flex")))
    (message "Parse 'md:flex': %S" result)
    (cl-assert (equal (plist-get result :variants) '("md")))
    (cl-assert (equal (plist-get result :utility) "flex")))
  
  ;; Test with state variant
  (let ((result (ecss-tailwind-parse-class "hover:bg-blue-500")))
    (message "Parse 'hover:bg-blue-500': %S" result)
    (cl-assert (equal (plist-get result :variants) '("hover")))
    (cl-assert (equal (plist-get result :property) "bg"))
    (cl-assert (equal (plist-get result :value) "blue-500")))
  
  ;; Test with multiple variants
  (let ((result (ecss-tailwind-parse-class "md:hover:bg-red-500")))
    (message "Parse 'md:hover:bg-red-500': %S" result)
    (cl-assert (equal (plist-get result :variants) '("md" "hover")))
    (cl-assert (equal (plist-get result :utility) "bg-red-500")))
  
  ;; Test arbitrary value
  (let ((result (ecss-tailwind-parse-class "bg-[#1da1f2]")))
    (message "Parse 'bg-[#1da1f2]': %S" result)
    (cl-assert (equal (plist-get result :property) "bg"))
    (cl-assert (equal (plist-get result :arbitrary) "#1da1f2")))
  
  ;; Test complex arbitrary value
  (let ((result (ecss-tailwind-parse-class "w-[calc(100%-2rem)]")))
    (message "Parse 'w-[calc(100%%-2rem)]': %S" result)
    (cl-assert (equal (plist-get result :property) "w"))
    (cl-assert (equal (plist-get result :arbitrary) "calc(100%-2rem)")))
  
  (message "\n✓ All parsing tests passed!\n"))

(defun test-tailwind-validation ()
  "Test Tailwind class validation."
  (message "\n=== Testing Tailwind Class Validation ===\n")
  
  ;; Valid classes
  (cl-assert (ecss-tailwind-class-p "flex"))
  (message "✓ 'flex' is valid")
  
  (cl-assert (ecss-tailwind-class-p "bg-red-500"))
  (message "✓ 'bg-red-500' is valid")
  
  (cl-assert (ecss-tailwind-class-p "md:flex"))
  (message "✓ 'md:flex' is valid")
  
  (cl-assert (ecss-tailwind-class-p "hover:bg-blue-500"))
  (message "✓ 'hover:bg-blue-500' is valid")
  
  (cl-assert (ecss-tailwind-class-p "lg:hover:text-xl"))
  (message "✓ 'lg:hover:text-xl' is valid")
  
  (cl-assert (ecss-tailwind-class-p "dark:bg-gray-900"))
  (message "✓ 'dark:bg-gray-900' is valid")
  
  (cl-assert (ecss-tailwind-class-p "bg-[#1da1f2]"))
  (message "✓ 'bg-[#1da1f2]' is valid")
  
  ;; Invalid classes (should be nil)
  (cl-assert (not (ecss-tailwind-class-p "notarealclass-xyz-999")))
  (message "✓ 'notarealclass-xyz-999' is correctly invalid")
  
  (cl-assert (not (ecss-tailwind-class-p "")))
  (message "✓ Empty string is correctly invalid")
  
  (message "\n✓ All validation tests passed!\n"))

(defun test-tailwind-dom-integration ()
  "Test Tailwind DOM integration."
  (message "\n=== Testing Tailwind DOM Integration ===\n")
  
  ;; Create a test DOM
  (let ((dom '(div ((class . "flex items-center justify-center bg-red-500 hover:bg-red-600"))
                   (span ((class . "text-lg font-bold md:text-xl")) "Hello")
                   (button ((class . "px-4 py-2 bg-blue-500 hover:bg-blue-600")) "Click"))))
    
    ;; Test querying by Tailwind class
    (let ((flex-nodes (ecss-dom-query-tailwind dom "flex")))
      (message "Nodes with 'flex': %d" (length flex-nodes))
      (cl-assert (= (length flex-nodes) 1)))
    
    (let ((bg-nodes (ecss-dom-query-tailwind dom "bg-red-500")))
      (message "Nodes with 'bg-red-500': %d" (length bg-nodes))
      (cl-assert (= (length bg-nodes) 1)))
    
    ;; Test pattern matching
    (let ((bg-pattern-nodes (ecss-dom-query-tailwind-pattern dom "^bg-")))
      (message "Nodes with bg- pattern: %d" (length bg-pattern-nodes))
      (cl-assert (>= (length bg-pattern-nodes) 2)))
    
    (let ((hover-nodes (ecss-dom-query-tailwind-pattern dom "^hover:")))
      (message "Nodes with hover: pattern: %d" (length hover-nodes))
      (cl-assert (>= (length hover-nodes) 2)))
    
    (message "\n✓ All DOM integration tests passed!\n")))

(defun test-tailwind-utility-functions ()
  "Test Tailwind utility functions."
  (message "\n=== Testing Tailwind Utility Functions ===\n")
  
  ;; Test variant extraction
  (let ((variants (ecss-tailwind-get-variants "md:hover:bg-red-500")))
    (message "Variants of 'md:hover:bg-red-500': %S" variants)
    (cl-assert (equal variants '("md" "hover"))))
  
  ;; Test utility extraction
  (let ((utility (ecss-tailwind-get-utility "md:hover:bg-red-500")))
    (message "Utility of 'md:hover:bg-red-500': %s" utility)
    (cl-assert (equal utility "bg-red-500")))
  
  ;; Test property extraction
  (let ((property (ecss-tailwind-get-property "text-lg")))
    (message "Property of 'text-lg': %s" property)
    (cl-assert (equal property "text")))
  
  ;; Test has-variant-p
  (cl-assert (ecss-tailwind-has-variant-p "md:flex" "md"))
  (message "✓ 'md:flex' has 'md' variant")
  
  (cl-assert (not (ecss-tailwind-has-variant-p "flex" "md")))
  (message "✓ 'flex' does not have 'md' variant")
  
  ;; Test has-responsive-p
  (cl-assert (ecss-tailwind-has-responsive-p "lg:text-xl"))
  (message "✓ 'lg:text-xl' has responsive prefix")
  
  (cl-assert (not (ecss-tailwind-has-responsive-p "hover:bg-red-500")))
  (message "✓ 'hover:bg-red-500' does not have responsive prefix")
  
  ;; Test has-state-variant-p
  (cl-assert (ecss-tailwind-has-state-variant-p "hover:bg-blue-500"))
  (message "✓ 'hover:bg-blue-500' has state variant")
  
  (cl-assert (not (ecss-tailwind-has-state-variant-p "md:flex")))
  (message "✓ 'md:flex' does not have state variant")
  
  (message "\n✓ All utility function tests passed!\n"))

(defun test-tailwind-class-manipulation ()
  "Test Tailwind class manipulation on DOM nodes."
  (message "\n=== Testing Tailwind Class Manipulation ===\n")
  
  ;; Create a test node
  (let ((node '(div ((class . "flex items-center")))))
    
    ;; Test adding a class
    (ecss-tailwind-add-class node "bg-red-500")
    (cl-assert (ecss-dom-has-class node "bg-red-500"))
    (message "✓ Added 'bg-red-500' class")
    
    ;; Test removing a class
    (ecss-tailwind-remove-class node "flex")
    (cl-assert (not (ecss-dom-has-class node "flex")))
    (message "✓ Removed 'flex' class")
    
    ;; Test toggling a class
    (ecss-tailwind-toggle-class node "hidden")
    (cl-assert (ecss-dom-has-class node "hidden"))
    (message "✓ Toggled 'hidden' class (now present)")
    
    (ecss-tailwind-toggle-class node "hidden")
    (cl-assert (not (ecss-dom-has-class node "hidden")))
    (message "✓ Toggled 'hidden' class (now absent)")
    
    ;; Test replacing a class
    (ecss-tailwind-replace-class node "bg-red-500" "bg-blue-500")
    (cl-assert (not (ecss-dom-has-class node "bg-red-500")))
    (cl-assert (ecss-dom-has-class node "bg-blue-500"))
    (message "✓ Replaced 'bg-red-500' with 'bg-blue-500'")
    
    (message "\n✓ All class manipulation tests passed!\n")))

(defun test-tailwind-get-classes-by-property ()
  "Test getting classes by property."
  (message "\n=== Testing Get Classes By Property ===\n")
  
  ;; Create a test node with multiple background classes
  (let ((node '(div ((class . "bg-red-500 text-lg hover:bg-red-600 p-4 bg-opacity-50")))))
    
    (let ((bg-classes (ecss-tailwind-get-classes-by-property node "bg")))
      (message "Background classes: %S" bg-classes)
      (cl-assert (member "bg-red-500" bg-classes))
      (cl-assert (member "bg-opacity-50" bg-classes))
      (message "✓ Found background classes correctly"))
    
    (let ((text-classes (ecss-tailwind-get-classes-by-property node "text")))
      (message "Text classes: %S" text-classes)
      (cl-assert (member "text-lg" text-classes))
      (message "✓ Found text classes correctly"))
    
    (let ((p-classes (ecss-tailwind-get-classes-by-property node "p")))
      (message "Padding classes: %S" p-classes)
      (cl-assert (member "p-4" p-classes))
      (message "✓ Found padding classes correctly"))
    
    (message "\n✓ All property-based class retrieval tests passed!\n")))

(defun test-tailwind-describe ()
  "Test Tailwind class description."
  (message "\n=== Testing Tailwind Class Description ===\n")
  
  (let ((description (ecss-tailwind-describe-class "md:hover:bg-red-500")))
    (message "%s" description))
  
  (let ((description (ecss-tailwind-describe-class "flex")))
    (message "%s" description))
  
  (let ((description (ecss-tailwind-describe-class "bg-[#1da1f2]")))
    (message "%s" description))
  
  (message "\n✓ Description tests completed!\n"))

;;; Run all tests

(defun run-all-tailwind-tests ()
  "Run all Tailwind CSS tests."
  (interactive)
  (message "\n========================================")
  (message "Running Tailwind CSS Tests")
  (message "========================================\n")
  
  (test-tailwind-parse)
  (test-tailwind-validation)
  (test-tailwind-utility-functions)
  (test-tailwind-dom-integration)
  (test-tailwind-class-manipulation)
  (test-tailwind-get-classes-by-property)
  (test-tailwind-describe)
  
  (message "\n========================================")
  (message "✓✓✓ ALL TESTS PASSED! ✓✓✓")
  (message "========================================\n"))

(provide 'test-tailwind)

;;; test-tailwind.el ends here
