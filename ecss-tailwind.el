;;; ecss-tailwind.el --- Tailwind CSS support for ECSS -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ECSS Contributors
;; Keywords: css, tailwind, utility-first
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; 这个库为ECSS添加Tailwind CSS支持。
;; Tailwind CSS是一个实用优先的CSS框架，使用大量的实用类来构建界面。
;;
;; 主要功能：
;; - 识别和解析Tailwind CSS类名模式
;; - 支持响应式前缀（sm:, md:, lg:, xl:, 2xl:）
;; - 支持状态变体（hover:, focus:, active:, disabled:等）
;; - 支持任意值语法（如 bg-[#1da1f2]）
;; - 提供Tailwind类名验证和匹配功能
;; - 为DOM节点添加和移除Tailwind类
;;
;; 使用示例：
;;
;;   ;; 检查是否为有效的Tailwind类
;;   (ecss-tailwind-class-p "bg-red-500")  ; => t
;;   (ecss-tailwind-class-p "hover:text-lg")  ; => t
;;
;;   ;; 解析Tailwind类
;;   (ecss-tailwind-parse-class "md:hover:bg-blue-500")
;;   ;; => (:variants ("md" "hover") :utility "bg-blue-500" :property "bg" :value "blue-500")
;;
;;   ;; 在DOM中查询具有特定Tailwind类的节点
;;   (ecss-dom-query-tailwind dom "flex")
;;   (ecss-dom-query-tailwind dom "bg-red-500")
;;
;;   ;; 添加Tailwind类到DOM节点
;;   (ecss-tailwind-add-class node "hover:bg-blue-500")

;;; Code:

(require 'cl-lib)
(require 'ecss-dom)

;;; Tailwind CSS patterns and utilities

(defconst ecss-tailwind-responsive-prefixes
  '("sm" "md" "lg" "xl" "2xl")
  "Tailwind CSS响应式断点前缀。")

(defconst ecss-tailwind-state-variants
  '("hover" "focus" "active" "visited" "disabled" "checked"
    "focus-within" "focus-visible" "group-hover" "group-focus"
    "first" "last" "odd" "even" "required" "invalid" "placeholder-shown"
    "before" "after" "selection" "marker" "file")
  "Tailwind CSS状态变体。")

(defconst ecss-tailwind-utility-prefixes
  '(;; Layout
    "container" "box" "block" "inline" "flex" "grid" "table" "hidden"
    "float" "clear" "object" "overflow" "overscroll" "position"
    "top" "right" "bottom" "left" "inset" "visible" "invisible" "z"
    
    ;; Flexbox & Grid
    "basis" "direction" "wrap" "grow" "shrink" "order"
    "cols" "col" "rows" "row" "gap" "justify" "content" "items" "self" "place"
    
    ;; Spacing
    "p" "px" "py" "pt" "pr" "pb" "pl" "ps" "pe"
    "m" "mx" "my" "mt" "mr" "mb" "ml" "ms" "me"
    "space"
    
    ;; Sizing
    "w" "min-w" "max-w" "h" "min-h" "max-h" "size"
    
    ;; Typography
    "font" "text" "leading" "tracking" "break" "hyphens"
    "antialiased" "subpixel-antialiased"
    "italic" "not-italic" "normal-case" "uppercase" "lowercase" "capitalize"
    "underline" "overline" "line-through" "no-underline"
    "indent" "align" "whitespace" "truncate"
    
    ;; Backgrounds
    "bg" "from" "via" "to" "gradient"
    
    ;; Borders
    "border" "divide" "outline" "ring"
    "rounded"
    
    ;; Effects
    "shadow" "opacity" "mix" "blur" "brightness" "contrast"
    "drop-shadow" "grayscale" "hue-rotate" "invert" "saturate" "sepia"
    "backdrop"
    
    ;; Filters
    "filter" "backdrop-filter"
    
    ;; Tables
    "border-collapse" "border-separate" "table-auto" "table-fixed"
    
    ;; Transitions & Animation
    "transition" "duration" "ease" "delay" "animate"
    
    ;; Transforms
    "scale" "rotate" "translate" "skew" "origin"
    
    ;; Interactivity
    "appearance" "cursor" "caret" "pointer-events" "resize"
    "scroll" "snap" "touch" "select" "will-change"
    
    ;; SVG
    "fill" "stroke"
    
    ;; Accessibility
    "sr")
  "Tailwind CSS实用类前缀列表。")

(defconst ecss-tailwind-standalone-utilities
  '("container" "flex" "grid" "block" "inline" "hidden" "visible" "invisible"
    "static" "fixed" "absolute" "relative" "sticky"
    "truncate" "italic" "not-italic" "antialiased" "subpixel-antialiased"
    "uppercase" "lowercase" "capitalize" "normal-case")
  "独立的Tailwind CSS实用类（不需要值）。")

;;; Tailwind class parsing

(defun ecss-tailwind-parse-class (class-name)
  "解析Tailwind CSS类名，返回其组成部分。
返回一个plist，包含：
  :variants - 变体列表（如响应式前缀和状态）
  :utility - 完整的实用类名（不含变体）
  :property - 属性名称（如'bg'从'bg-red-500'）
  :value - 值部分（如'red-500'从'bg-red-500'）
  :arbitrary - 如果使用任意值语法，包含方括号内的值

示例：
  (ecss-tailwind-parse-class \"md:hover:bg-red-500\")
  => (:variants (\"md\" \"hover\") :utility \"bg-red-500\" :property \"bg\" :value \"red-500\")"
  (let ((parts (split-string class-name ":"))
        (variants '())
        (utility nil)
        (property nil)
        (value nil)
        (arbitrary nil))
    
    ;; 分离变体和实用类
    (when (> (length parts) 1)
      (setq variants (butlast parts)
            utility (car (last parts)))
      ;; 过滤掉空字符串
      (setq variants (cl-remove-if #'string-empty-p variants)))
    
    (when (= (length parts) 1)
      (setq utility (car parts)))
    
    ;; 解析实用类
    (when utility
      ;; 检查任意值语法 [...]
      (if (string-match "\\(.*\\)\\[\\([^]]+\\)\\]" utility)
          (progn
            (setq property (match-string 1 utility)
                  arbitrary (match-string 2 utility))
            ;; 移除property末尾的 '-' 如果存在
            (when (string-suffix-p "-" property)
              (setq property (substring property 0 -1))))
        ;; 标准格式：property-value
        (let ((hyphen-pos (string-match "-" utility)))
          (if hyphen-pos
              (setq property (substring utility 0 hyphen-pos)
                    value (substring utility (1+ hyphen-pos)))
            ;; 独立实用类（无连字符）
            (setq property utility)))))
    
    (list :variants variants
          :utility utility
          :property property
          :value value
          :arbitrary arbitrary)))

(defun ecss-tailwind-class-p (class-name)
  "检查CLASS-NAME是否是有效的Tailwind CSS类名。
这是一个简化的验证，检查类名是否符合Tailwind的命名模式。"
  (when (and class-name (stringp class-name))
    (let* ((parsed (ecss-tailwind-parse-class class-name))
           (variants (plist-get parsed :variants))
           (property (plist-get parsed :property))
           (arbitrary (plist-get parsed :arbitrary)))
      
      ;; 检查变体是否有效
      (let ((valid-variants t))
        (dolist (variant variants)
          (unless (or (member variant ecss-tailwind-responsive-prefixes)
                      (member variant ecss-tailwind-state-variants)
                      ;; 允许dark和自定义变体
                      (string= variant "dark")
                      (string-prefix-p "group-" variant)
                      (string-prefix-p "peer-" variant))
            (setq valid-variants nil)))
        
        ;; 检查属性是否有效
        (and valid-variants
             (or
              ;; 独立实用类
              (member property ecss-tailwind-standalone-utilities)
              ;; 有前缀的实用类
              (cl-some (lambda (prefix)
                         (string-prefix-p prefix property))
                       ecss-tailwind-utility-prefixes)
              ;; 任意值
              arbitrary))))))

(defun ecss-tailwind-get-variants (class-name)
  "从Tailwind类名中提取所有变体。"
  (plist-get (ecss-tailwind-parse-class class-name) :variants))

(defun ecss-tailwind-get-utility (class-name)
  "从Tailwind类名中提取实用类部分（不含变体）。"
  (plist-get (ecss-tailwind-parse-class class-name) :utility))

(defun ecss-tailwind-get-property (class-name)
  "从Tailwind类名中提取属性名称。"
  (plist-get (ecss-tailwind-parse-class class-name) :property))

(defun ecss-tailwind-has-variant-p (class-name variant)
  "检查Tailwind类名是否包含指定的变体。"
  (member variant (ecss-tailwind-get-variants class-name)))

(defun ecss-tailwind-has-responsive-p (class-name)
  "检查Tailwind类名是否有响应式前缀。"
  (let ((variants (ecss-tailwind-get-variants class-name)))
    (cl-some (lambda (v) (member v ecss-tailwind-responsive-prefixes))
             variants)))

(defun ecss-tailwind-has-state-variant-p (class-name)
  "检查Tailwind类名是否有状态变体。"
  (let ((variants (ecss-tailwind-get-variants class-name)))
    (cl-some (lambda (v) (member v ecss-tailwind-state-variants))
             variants)))

;;; DOM integration

(defun ecss-dom-node-has-tailwind-class-p (node class-name)
  "检查DOM节点是否有指定的Tailwind类。
支持精确匹配和模式匹配。"
  (when (and node (listp node))
    (let* ((attrs (dom-attributes node))
           (class-attr (cdr (assq 'class attrs))))
      (when class-attr
        (let ((classes (split-string class-attr)))
          (member class-name classes))))))

(defun ecss-dom-query-tailwind (dom tailwind-class)
  "在DOM树中查询所有具有指定Tailwind类的节点。
TAILWIND-CLASS是要查询的Tailwind类名。

示例：
  (ecss-dom-query-tailwind dom \"flex\")
  (ecss-dom-query-tailwind dom \"bg-red-500\")
  (ecss-dom-query-tailwind dom \"md:hover:text-lg\")"
  (let ((results '()))
    (ecss-dom-walk
     (lambda (node)
       (when (ecss-dom-node-has-tailwind-class-p node tailwind-class)
         (push node results)))
     dom)
    (nreverse results)))

(defun ecss-dom-query-tailwind-pattern (dom pattern)
  "在DOM树中查询匹配Tailwind模式的节点。
PATTERN可以是：
  - 属性前缀，如'bg'匹配所有背景类
  - 变体，如'hover:'匹配所有hover变体
  - 正则表达式

示例：
  (ecss-dom-query-tailwind-pattern dom \"^bg-\")  ; 所有背景类
  (ecss-dom-query-tailwind-pattern dom \"^hover:\") ; 所有hover类"
  (let ((results '())
        (regexp (if (stringp pattern) pattern (regexp-quote pattern))))
    (ecss-dom-walk
     (lambda (node)
       (when (and node (listp node))
         (let* ((attrs (dom-attributes node))
                (class-attr (cdr (assq 'class attrs))))
           (when class-attr
             (let ((classes (split-string class-attr)))
               (when (cl-some (lambda (class)
                                (string-match-p regexp class))
                              classes)
                 (push node results)))))))
     dom)
    (nreverse results)))

(defun ecss-tailwind-add-class (node class-name)
  "为DOM节点添加Tailwind CSS类。
如果类名不是有效的Tailwind类，会发出警告但仍然添加。"
  (unless (ecss-tailwind-class-p class-name)
    (message "Warning: '%s' may not be a valid Tailwind class" class-name))
  (ecss-dom-add-class node class-name))

(defun ecss-tailwind-remove-class (node class-name)
  "从DOM节点移除Tailwind CSS类。"
  (ecss-dom-remove-class node class-name))

(defun ecss-tailwind-toggle-class (node class-name)
  "切换DOM节点的Tailwind CSS类。"
  (unless (ecss-tailwind-class-p class-name)
    (message "Warning: '%s' may not be a valid Tailwind class" class-name))
  (ecss-dom-toggle-class node class-name))

(defun ecss-tailwind-replace-class (node old-class new-class)
  "替换DOM节点的Tailwind类。"
  (when (ecss-dom-has-class node old-class)
    (ecss-dom-remove-class node old-class)
    (ecss-tailwind-add-class node new-class)))

(defun ecss-tailwind-get-classes-by-property (node property)
  "获取DOM节点中指定属性的所有Tailwind类。
例如，获取所有'bg'（背景）类或所有'text'（文本）类。"
  (when (and node (listp node))
    (let* ((attrs (dom-attributes node))
           (class-attr (cdr (assq 'class attrs)))
           (results '()))
      (when class-attr
        (let ((classes (split-string class-attr)))
          (dolist (class classes)
            (when (ecss-tailwind-class-p class)
              (let* ((parsed (ecss-tailwind-parse-class class))
                     (class-property (plist-get parsed :property)))
                (when (string= class-property property)
                  (push class results)))))))
      (nreverse results))))

(defun ecss-tailwind-filter-classes (classes &optional filter-fn)
  "过滤Tailwind类列表。
FILTER-FN是一个函数，接受解析后的类信息，返回t表示保留。
如果FILTER-FN为nil，只返回有效的Tailwind类。"
  (cl-remove-if-not
   (lambda (class)
     (and (ecss-tailwind-class-p class)
          (or (null filter-fn)
              (funcall filter-fn (ecss-tailwind-parse-class class)))))
   classes))

;;; Utility functions

(defun ecss-tailwind-describe-class (class-name)
  "描述Tailwind类的详细信息。
返回一个人类可读的描述字符串。"
  (let* ((parsed (ecss-tailwind-parse-class class-name))
         (variants (plist-get parsed :variants))
         (utility (plist-get parsed :utility))
         (property (plist-get parsed :property))
         (value (plist-get parsed :value))
         (arbitrary (plist-get parsed :arbitrary)))
    (format "Tailwind Class: %s\n  Variants: %s\n  Utility: %s\n  Property: %s\n  Value: %s\n  Arbitrary: %s\n  Valid: %s"
            class-name
            (if variants (mapconcat #'identity variants ", ") "none")
            (or utility "none")
            (or property "none")
            (or value "none")
            (or arbitrary "none")
            (if (ecss-tailwind-class-p class-name) "yes" "no"))))

(provide 'ecss-tailwind)

;;; ecss-tailwind.el ends here
