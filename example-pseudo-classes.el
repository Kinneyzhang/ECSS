;;; example-pseudo-classes.el --- Examples demonstrating pseudo-class selectors -*- lexical-binding: t; -*-

;; This file contains practical examples of pseudo-class selector usage

;;; Code:

(require 'ecss-selector)
(require 'ecss-dom)

(defun example-create-blog-dom ()
  "Create a sample blog DOM tree."
  '(html nil
         (head nil
               (title nil "My Blog"))
         (body nil
               (header nil
                       (h1 nil "My Awesome Blog")
                       (nav nil
                            (a ((href . "/")) "Home")
                            (a ((href . "/about")) "About")
                            (a ((href . "/contact")) "Contact")))
               (main nil
                     (article ((class . "post featured"))
                              (h2 nil "First Post")
                              (p nil "This is the first paragraph.")
                              (p nil "This is the second paragraph.")
                              (p nil "This is the third paragraph."))
                     (article ((class . "post"))
                              (h2 nil "Second Post")
                              (p nil "Another post content."))
                     (article ((class . "post"))
                              (h2 nil "Third Post")
                              (p nil "More content here.")))
               (aside nil
                      (div ((class . "widget"))
                           (h3 nil "Recent Posts")
                           (ul nil
                               (li nil "Post 1")
                               (li nil "Post 2")
                               (li nil "Post 3")
                               (li nil "Post 4")
                               (li nil "Post 5")))
                      (div ((class . "widget"))
                           (h3 nil "Categories")
                           (ul nil
                               (li nil "Tech")
                               (li nil "Life")
                               (li nil "Travel"))))
               (footer nil
                       (p nil "© 2024 My Blog")))))

(defun example-first-and-last-child ()
  "Example: Select first and last children."
  (message "\n=== First and Last Child Examples ===\n")
  
  (let ((dom (example-create-blog-dom)))
    
    ;; Select the first article (featured post)
    (message "Selecting first article using :first-child:")
    (let ((results (ecss-dom-query-selector-all dom "article:first-child")))
      (message "  Found %d article(s)" (length results))
      (dolist (node results)
        (let ((classes (cdr (assq 'class (dom-attributes node)))))
          (message "  - Article with class: %s" (or classes "none")))))
    
    ;; Select the last article
    (message "\nSelecting last article using :last-child:")
    (let ((results (ecss-dom-query-selector-all dom "article:last-child")))
      (message "  Found %d article(s)" (length results))
      (dolist (node results)
        (let ((h2 (dom-by-tag node 'h2)))
          (when h2
            (message "  - %s" (dom-text h2))))))
    
    ;; Select first paragraph in each article
    (message "\nSelecting first paragraph in each container using :first-of-type:")
    (let ((results (ecss-dom-query-selector-all dom "p:first-of-type")))
      (message "  Found %d paragraph(s)" (length results))
      (dolist (node results)
        (message "  - %s" (substring (dom-text node) 0 (min 30 (length (dom-text node)))))))
    
    ;; Select last paragraph in each article
    (message "\nSelecting last paragraph in each container using :last-of-type:")
    (let ((results (ecss-dom-query-selector-all dom "p:last-of-type")))
      (message "  Found %d paragraph(s)" (length results))))
  
  (message "\n"))

(defun example-nth-child ()
  "Example: Select elements using :nth-child()."
  (message "\n=== Nth-Child Examples ===\n")
  
  (let ((dom (example-create-blog-dom)))
    
    ;; Select odd list items
    (message "Selecting odd list items using :nth-child(odd):")
    (let ((results (ecss-dom-query-selector-all dom "li:nth-child(odd)")))
      (message "  Found %d items" (length results))
      (dolist (node results)
        (message "  - %s" (dom-text node))))
    
    ;; Select even list items
    (message "\nSelecting even list items using :nth-child(even):")
    (let ((results (ecss-dom-query-selector-all dom "li:nth-child(even)")))
      (message "  Found %d items" (length results))
      (dolist (node results)
        (message "  - %s" (dom-text node))))
    
    ;; Select every 3rd list item starting from the 1st
    (message "\nSelecting every 3rd item using :nth-child(3n+1):")
    (let ((results (ecss-dom-query-selector-all dom "li:nth-child(3n+1)")))
      (message "  Found %d items" (length results))
      (dolist (node results)
        (message "  - %s" (dom-text node))))
    
    ;; Select the second article
    (message "\nSelecting second article using :nth-of-type(2):")
    (let ((results (ecss-dom-query-selector-all dom "article:nth-of-type(2)")))
      (message "  Found %d article(s)" (length results))
      (dolist (node results)
        (let ((h2 (dom-by-tag node 'h2)))
          (when h2
            (message "  - %s" (dom-text h2)))))))
  
  (message "\n"))

(defun example-only-child ()
  "Example: Select only children."
  (message "\n=== Only-Child Examples ===\n")
  
  (let ((dom (example-create-blog-dom)))
    
    ;; Select elements that are the only child
    (message "Selecting title as only child in head:")
    (let ((results (ecss-dom-query-selector-all dom "title:only-child")))
      (message "  Found %d element(s)" (length results))
      (dolist (node results)
        (message "  - %s" (dom-text node))))
    
    ;; Select h1 that is only of its type
    (message "\nSelecting h1 as only of type:")
    (let ((results (ecss-dom-query-selector-all dom "h1:only-of-type")))
      (message "  Found %d element(s)" (length results))
      (dolist (node results)
        (message "  - %s" (dom-text node))))
    
    ;; Select footer p as only child
    (message "\nSelecting footer paragraph as only child:")
    (let ((results (ecss-dom-query-selector-all dom "footer p:only-child")))
      (message "  Found %d element(s)" (length results))
      (dolist (node results)
        (message "  - %s" (dom-text node)))))
  
  (message "\n"))

(defun example-not-selector ()
  "Example: Use :not() to exclude elements."
  (message "\n=== :not() Examples ===\n")
  
  (let ((dom (example-create-blog-dom)))
    
    ;; Select all articles except the featured one
    (message "Selecting articles that are not featured:")
    (let ((results (ecss-dom-query-selector-all dom "article:not(.featured)")))
      (message "  Found %d article(s)" (length results))
      (dolist (node results)
        (let ((h2 (dom-by-tag node 'h2)))
          (when h2
            (message "  - %s" (dom-text h2))))))
    
    ;; Select all p elements that are not first children
    (message "\nSelecting paragraphs that are not first children:")
    (let ((results (ecss-dom-query-selector-all dom "p:not(:first-child)")))
      (message "  Found %d paragraph(s)" (length results))
      (dolist (node results)
        (let ((text (dom-text node)))
          (message "  - %s" (substring text 0 (min 30 (length text)))))))
    
    ;; Select all li elements that are not last children
    (message "\nSelecting list items that are not last children:")
    (let ((results (ecss-dom-query-selector-all dom "li:not(:last-child)")))
      (message "  Found %d item(s)" (length results))))
  
  (message "\n"))

(defun example-empty-selector ()
  "Example: Select empty elements."
  (message "\n=== :empty Examples ===\n")
  
  ;; Create a DOM with some empty elements
  (let ((dom '(div nil
                   (div ((class . "empty")))
                   (div ((class . "whitespace")) "   ")
                   (div ((class . "has-content")) "Content")
                   (div ((class . "has-child"))
                        (p nil "Child")))))
    
    (message "Selecting empty divs:")
    (let ((results (ecss-dom-query-selector-all dom "div:empty")))
      (message "  Found %d empty div(s)" (length results))
      (dolist (node results)
        (let ((classes (cdr (assq 'class (dom-attributes node)))))
          (message "  - Class: %s" (or classes "none")))))
    
    (message "\nSelecting non-empty divs using :not(:empty):")
    (let ((results (ecss-dom-query-selector-all dom "div:not(:empty)")))
      (message "  Found %d non-empty div(s)" (length results))
      (dolist (node results)
        (let ((classes (cdr (assq 'class (dom-attributes node)))))
          (message "  - Class: %s" (or classes "none"))))))
  
  (message "\n"))

(defun example-styling-with-pseudo-classes ()
  "Example: Apply styles based on pseudo-class selectors."
  (message "\n=== Styling with Pseudo-classes ===\n")
  
  (let ((dom (example-create-blog-dom)))
    
    ;; Style first paragraph in each article
    (message "Styling first paragraph in articles:")
    (let ((results (ecss-dom-apply-style 
                    dom 
                    "article p:first-of-type"
                    '((font-weight . "bold")
                      (font-size . "18px")))))
      (message "  Applied bold style to %d paragraph(s)" (length results)))
    
    ;; Style last list item
    (message "\nStyling last list item:")
    (let ((results (ecss-dom-apply-style
                    dom
                    "li:last-child"
                    '((border-bottom . "none")))))
      (message "  Removed bottom border from %d item(s)" (length results)))
    
    ;; Style odd rows in lists
    (message "\nStyling odd list items (zebra striping):")
    (let ((results (ecss-dom-apply-style
                    dom
                    "li:nth-child(odd)"
                    '((background-color . "#f5f5f5")))))
      (message "  Applied background to %d item(s)" (length results)))
    
    ;; Style featured article differently
    (message "\nStyling featured article:")
    (let ((results (ecss-dom-apply-style
                    dom
                    "article.featured:first-child"
                    '((border-left . "4px solid #007bff")
                      (padding-left . "20px")))))
      (message "  Applied featured style to %d article(s)" (length results))))
  
  (message "\n"))

(defun run-all-pseudo-class-examples ()
  "Run all pseudo-class examples."
  (interactive)
  (message "\n========================================")
  (message "Pseudo-class Selector Examples")
  (message "========================================")
  
  (example-first-and-last-child)
  (example-nth-child)
  (example-only-child)
  (example-not-selector)
  (example-empty-selector)
  (example-styling-with-pseudo-classes)
  
  (message "========================================")
  (message "All examples completed!")
  (message "========================================\n"))

(provide 'example-pseudo-classes)

;;; example-pseudo-classes.el ends here
