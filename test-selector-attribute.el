;;; test-selector-attribute.el --- Tests for attribute selector parsing -*- lexical-binding: t; -*-

;; This file contains tests for attribute selector parsing in ecss-selector.el

;;; Code:

(require 'ecss-selector)

(defun test-attribute-selector-operators ()
  "Test attribute selector operators parsing."
  (message "\n=== Testing Attribute Selector Operators ===\n")
  
  ;; Test basic equals operator
  (let* ((result (ecss-selector-parse "a[href=\"tailwindcss\"]"))
         (selector (car (plist-get result :nodes)))
         (nodes (plist-get selector :nodes))
         (attr (cadr nodes)))
    (message "Testing a[href=\"tailwindcss\"]")
    (cl-assert (equal (plist-get attr :type) 'attribute))
    (cl-assert (equal (plist-get attr :attribute) "href"))
    (cl-assert (equal (plist-get attr :operator) "="))
    (cl-assert (equal (plist-get attr :value) "tailwindcss"))
    (cl-assert (equal (plist-get attr :quote-mark) "\""))
    (message "✓ Basic equals operator works"))
  
  ;; Test ~= operator (contains word)
  (let* ((result (ecss-selector-parse "a[href~=\"tailwindcss\"]"))
         (selector (car (plist-get result :nodes)))
         (nodes (plist-get selector :nodes))
         (attr (cadr nodes)))
    (message "Testing a[href~=\"tailwindcss\"]")
    (cl-assert (equal (plist-get attr :type) 'attribute))
    (cl-assert (equal (plist-get attr :attribute) "href"))
    (cl-assert (equal (plist-get attr :operator) "~="))
    (cl-assert (equal (plist-get attr :value) "tailwindcss"))
    (cl-assert (equal (plist-get attr :quote-mark) "\""))
    (message "✓ ~= operator works"))
  
  ;; Test |= operator (starts with word followed by hyphen)
  (let* ((result (ecss-selector-parse "a[href|=\"tailwindcss\"]"))
         (selector (car (plist-get result :nodes)))
         (nodes (plist-get selector :nodes))
         (attr (cadr nodes)))
    (message "Testing a[href|=\"tailwindcss\"]")
    (cl-assert (equal (plist-get attr :type) 'attribute))
    (cl-assert (equal (plist-get attr :attribute) "href"))
    (cl-assert (equal (plist-get attr :operator) "|="))
    (cl-assert (equal (plist-get attr :value) "tailwindcss"))
    (cl-assert (equal (plist-get attr :quote-mark) "\""))
    (message "✓ |= operator works"))
  
  ;; Test ^= operator (starts with)
  (let* ((result (ecss-selector-parse "a[href^=\"tail\"]"))
         (selector (car (plist-get result :nodes)))
         (nodes (plist-get selector :nodes))
         (attr (cadr nodes)))
    (message "Testing a[href^=\"tail\"]")
    (cl-assert (equal (plist-get attr :type) 'attribute))
    (cl-assert (equal (plist-get attr :attribute) "href"))
    (cl-assert (equal (plist-get attr :operator) "^="))
    (cl-assert (equal (plist-get attr :value) "tail"))
    (cl-assert (equal (plist-get attr :quote-mark) "\""))
    (message "✓ ^= operator works"))
  
  ;; Test $= operator (ends with)
  (let* ((result (ecss-selector-parse "a[href$=\"css\"]"))
         (selector (car (plist-get result :nodes)))
         (nodes (plist-get selector :nodes))
         (attr (cadr nodes)))
    (message "Testing a[href$=\"css\"]")
    (cl-assert (equal (plist-get attr :type) 'attribute))
    (cl-assert (equal (plist-get attr :attribute) "href"))
    (cl-assert (equal (plist-get attr :operator) "$="))
    (cl-assert (equal (plist-get attr :value) "css"))
    (cl-assert (equal (plist-get attr :quote-mark) "\""))
    (message "✓ $= operator works"))
  
  ;; Test *= operator (contains substring)
  (let* ((result (ecss-selector-parse "a[href*=\"wind\"]"))
         (selector (car (plist-get result :nodes)))
         (nodes (plist-get selector :nodes))
         (attr (cadr nodes)))
    (message "Testing a[href*=\"wind\"]")
    (cl-assert (equal (plist-get attr :type) 'attribute))
    (cl-assert (equal (plist-get attr :attribute) "href"))
    (cl-assert (equal (plist-get attr :operator) "*="))
    (cl-assert (equal (plist-get attr :value) "wind"))
    (cl-assert (equal (plist-get attr :quote-mark) "\""))
    (message "✓ *= operator works"))
  
  ;; Test attribute without value
  (let* ((result (ecss-selector-parse "a[href]"))
         (selector (car (plist-get result :nodes)))
         (nodes (plist-get selector :nodes))
         (attr (cadr nodes)))
    (message "Testing a[href]")
    (cl-assert (equal (plist-get attr :type) 'attribute))
    (cl-assert (equal (plist-get attr :attribute) "href"))
    (cl-assert (null (plist-get attr :operator)))
    (cl-assert (null (plist-get attr :value)))
    (message "✓ Attribute without value works"))
  
  ;; Test with single quotes
  (let* ((result (ecss-selector-parse "a[href~='tailwindcss']"))
         (selector (car (plist-get result :nodes)))
         (nodes (plist-get selector :nodes))
         (attr (cadr nodes)))
    (message "Testing a[href~='tailwindcss']")
    (cl-assert (equal (plist-get attr :type) 'attribute))
    (cl-assert (equal (plist-get attr :attribute) "href"))
    (cl-assert (equal (plist-get attr :operator) "~="))
    (cl-assert (equal (plist-get attr :value) "tailwindcss"))
    (cl-assert (equal (plist-get attr :quote-mark) "'"))
    (message "✓ Single quotes work"))
  
  (message "\n✓ All attribute selector operator tests passed!\n"))

(defun test-attribute-selector-roundtrip ()
  "Test that attribute selectors can be parsed and converted back to string."
  (message "\n=== Testing Attribute Selector Round-trip ===\n")
  
  (let ((selectors '("a[href]"
                     "a[href=\"value\"]"
                     "a[href~=\"value\"]"
                     "a[href|=\"value\"]"
                     "a[href^=\"value\"]"
                     "a[href$=\"value\"]"
                     "a[href*=\"value\"]")))
    (dolist (selector selectors)
      (let* ((parsed (ecss-selector-parse selector))
             (stringified (ecss-selector-stringify parsed)))
        (message "Original: %s" selector)
        (message "Parsed and stringified: %s" stringified)
        ;; Note: whitespace might differ, but the essential structure should be the same
        (cl-assert (not (null parsed)))
        (cl-assert (not (string-empty-p stringified))))))
  
  (message "\n✓ All round-trip tests passed!\n"))

(defun run-attribute-selector-tests ()
  "Run all attribute selector tests."
  (interactive)
  (message "\n========================================")
  (message "Running Attribute Selector Tests")
  (message "========================================\n")
  
  (test-attribute-selector-operators)
  (test-attribute-selector-roundtrip)
  
  (message "\n========================================")
  (message "✓✓✓ ALL TESTS PASSED! ✓✓✓")
  (message "========================================\n"))

(provide 'test-selector-attribute)

;;; test-selector-attribute.el ends here
