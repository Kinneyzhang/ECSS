;;; example-tailwind.el --- Tailwind CSS support usage examples -*- lexical-binding: t; -*-

;;; Commentary:

;; 这个文件展示了如何使用ECSS的Tailwind CSS支持功能。
;; This file demonstrates how to use ECSS's Tailwind CSS support features.

;;; Code:

(require 'ecss-selector)
(require 'ecss-dom)
(require 'ecss-tailwind)

;;; Example 1: 解析Tailwind类名 / Parse Tailwind class names

(message "\n=== 示例 1: 解析Tailwind类名 ===\n")

;; 解析简单的实用类
(let ((result (ecss-tailwind-parse-class "flex")))
  (message "解析 'flex':")
  (message "  实用类: %s" (plist-get result :utility))
  (message "  属性: %s" (plist-get result :property)))

;; 解析带值的类
(let ((result (ecss-tailwind-parse-class "bg-red-500")))
  (message "\n解析 'bg-red-500':")
  (message "  属性: %s" (plist-get result :property))
  (message "  值: %s" (plist-get result :value)))

;; 解析带响应式前缀的类
(let ((result (ecss-tailwind-parse-class "md:text-lg")))
  (message "\n解析 'md:text-lg':")
  (message "  变体: %S" (plist-get result :variants))
  (message "  实用类: %s" (plist-get result :utility)))

;; 解析复杂的类（多个变体）
(let ((result (ecss-tailwind-parse-class "lg:hover:bg-blue-500")))
  (message "\n解析 'lg:hover:bg-blue-500':")
  (message "  变体: %S" (plist-get result :variants))
  (message "  属性: %s" (plist-get result :property))
  (message "  值: %s" (plist-get result :value)))

;; 解析任意值语法
(let ((result (ecss-tailwind-parse-class "bg-[#1da1f2]")))
  (message "\n解析 'bg-[#1da1f2]':")
  (message "  属性: %s" (plist-get result :property))
  (message "  任意值: %s" (plist-get result :arbitrary)))

;;; Example 2: 验证Tailwind类名 / Validate Tailwind class names

(message "\n\n=== 示例 2: 验证Tailwind类名 ===\n")

(message "验证 'flex': %s" (if (ecss-tailwind-class-p "flex") "有效" "无效"))
(message "验证 'bg-red-500': %s" (if (ecss-tailwind-class-p "bg-red-500") "有效" "无效"))
(message "验证 'md:hover:text-xl': %s" (if (ecss-tailwind-class-p "md:hover:text-xl") "有效" "无效"))
(message "验证 'not-a-tailwind-class': %s" (if (ecss-tailwind-class-p "not-a-tailwind-class") "有效" "无效"))

;;; Example 3: 在DOM中查询Tailwind类 / Query Tailwind classes in DOM

(message "\n\n=== 示例 3: 在DOM中查询Tailwind类 ===\n")

;; 创建一个示例DOM结构
(let ((dom '(div ((class . "container mx-auto"))
                 (header ((class . "flex items-center justify-between p-4 bg-white shadow"))
                         (h1 ((class . "text-2xl font-bold text-gray-900")) "标题")
                         (nav ((class . "flex space-x-4"))
                              (a ((class . "text-blue-500 hover:text-blue-700")) "链接1")
                              (a ((class . "text-blue-500 hover:text-blue-700")) "链接2")))
                 (main ((class . "py-8"))
                       (div ((class . "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4"))
                            (div ((class . "bg-white rounded-lg shadow-md p-6 hover:shadow-lg"))
                                 (h2 ((class . "text-xl font-semibold mb-2")) "卡片1")
                                 (p ((class . "text-gray-600")) "内容..."))
                            (div ((class . "bg-white rounded-lg shadow-md p-6 hover:shadow-lg"))
                                 (h2 ((class . "text-xl font-semibold mb-2")) "卡片2")
                                 (p ((class . "text-gray-600")) "内容...")))))))

  ;; 查询具有特定Tailwind类的节点
  (message "查询具有 'flex' 类的节点:")
  (let ((nodes (ecss-dom-query-tailwind dom "flex")))
    (message "  找到 %d 个节点" (length nodes)))

  ;; 使用模式查询
  (message "\n查询所有带 'bg-' 前缀的类:")
  (let ((nodes (ecss-dom-query-tailwind-pattern dom "^bg-")))
    (message "  找到 %d 个节点" (length nodes)))

  (message "\n查询所有带 'hover:' 变体的类:")
  (let ((nodes (ecss-dom-query-tailwind-pattern dom "^hover:")))
    (message "  找到 %d 个节点" (length nodes)))

  ;; 结合CSS选择器和Tailwind类查询
  (message "\n查询所有 div 元素中的 Tailwind 类:")
  (let ((div-nodes (ecss-dom-query-selector-all dom "div")))
    (message "  找到 %d 个 div 元素" (length div-nodes))
    (dolist (node div-nodes)
      (let* ((attrs (dom-attributes node))
             (class-attr (cdr (assq 'class attrs))))
        (when class-attr
          (message "    类: %s" class-attr))))))

;;; Example 4: 操作DOM节点的Tailwind类 / Manipulate Tailwind classes on DOM nodes

(message "\n\n=== 示例 4: 操作DOM节点的Tailwind类 ===\n")

(let ((node '(div ((class . "flex items-center")))))
  (message "初始类: %s" (cdr (assq 'class (dom-attributes node))))
  
  ;; 添加类
  (ecss-tailwind-add-class node "bg-red-500")
  (message "添加 'bg-red-500' 后: %s" (cdr (assq 'class (dom-attributes node))))
  
  ;; 添加更多类
  (ecss-tailwind-add-class node "p-4")
  (ecss-tailwind-add-class node "rounded-lg")
  (message "添加更多类后: %s" (cdr (assq 'class (dom-attributes node))))
  
  ;; 移除类
  (ecss-tailwind-remove-class node "flex")
  (message "移除 'flex' 后: %s" (cdr (assq 'class (dom-attributes node))))
  
  ;; 替换类
  (ecss-tailwind-replace-class node "bg-red-500" "bg-blue-500")
  (message "将 'bg-red-500' 替换为 'bg-blue-500' 后: %s" 
           (cdr (assq 'class (dom-attributes node)))))

;;; Example 5: 获取特定属性的Tailwind类 / Get Tailwind classes by property

(message "\n\n=== 示例 5: 获取特定属性的Tailwind类 ===\n")

(let ((node '(div ((class . "bg-red-500 text-lg hover:bg-red-600 p-4 bg-opacity-50 text-center")))))
  (message "节点的所有类: %s" (cdr (assq 'class (dom-attributes node))))
  
  (let ((bg-classes (ecss-tailwind-get-classes-by-property node "bg")))
    (message "\n背景相关的类 (bg):")
    (dolist (class bg-classes)
      (message "  - %s" class)))
  
  (let ((text-classes (ecss-tailwind-get-classes-by-property node "text")))
    (message "\n文本相关的类 (text):")
    (dolist (class text-classes)
      (message "  - %s" class)))
  
  (let ((p-classes (ecss-tailwind-get-classes-by-property node "p")))
    (message "\n内边距相关的类 (p):")
    (dolist (class p-classes)
      (message "  - %s" class))))

;;; Example 6: 过滤和分析Tailwind类 / Filter and analyze Tailwind classes

(message "\n\n=== 示例 6: 过滤和分析Tailwind类 ===\n")

(let ((classes '("flex" "md:grid" "bg-red-500" "hover:bg-red-600" 
                 "text-lg" "lg:text-xl" "p-4" "not-tailwind")))
  
  (message "原始类列表: %S" classes)
  
  ;; 过滤出有效的Tailwind类
  (let ((valid-classes (ecss-tailwind-filter-classes classes)))
    (message "\n有效的Tailwind类:")
    (dolist (class valid-classes)
      (message "  - %s" class)))
  
  ;; 过滤出有响应式前缀的类
  (let ((responsive-classes (ecss-tailwind-filter-classes 
                             classes
                             (lambda (parsed)
                               (ecss-tailwind-has-responsive-p 
                                (plist-get parsed :utility))))))
    (message "\n带响应式前缀的类:")
    (dolist (class responsive-classes)
      (message "  - %s" class)))
  
  ;; 过滤出背景相关的类
  (let ((bg-classes (ecss-tailwind-filter-classes 
                     classes
                     (lambda (parsed)
                       (string-prefix-p "bg" (or (plist-get parsed :property) ""))))))
    (message "\n背景相关的类:")
    (dolist (class bg-classes)
      (message "  - %s" class))))

;;; Example 7: 描述Tailwind类 / Describe Tailwind classes

(message "\n\n=== 示例 7: 描述Tailwind类 ===\n")

(message "%s\n" (ecss-tailwind-describe-class "lg:hover:bg-blue-500"))
(message "%s\n" (ecss-tailwind-describe-class "text-[14px]"))

;;; Example 8: 实际应用场景 / Real-world use case

(message "\n=== 示例 8: 实际应用场景 - 构建响应式导航栏 ===\n")

;; 创建一个响应式导航栏的DOM结构
(let ((nav-dom '(nav ((id . "main-nav"))
                     (div ((class . "container mx-auto"))
                          (div ((class . "flex items-center justify-between py-4"))
                               (a ((class . "text-2xl font-bold text-gray-900") (href . "/")) "Logo")
                               (button ((class . "md:hidden p-2") (id . "mobile-menu-button"))
                                       (span ((class . "sr-only")) "打开菜单"))
                               (div ((class . "hidden md:flex md:space-x-4") (id . "menu-items"))
                                    (a ((class . "text-gray-600 hover:text-gray-900") (href . "/home")) "首页")
                                    (a ((class . "text-gray-600 hover:text-gray-900") (href . "/about")) "关于")
                                    (a ((class . "text-gray-600 hover:text-gray-900") (href . "/contact")) "联系")))))))
  
  (message "导航栏DOM结构已创建\n")
  
  ;; 查询所有响应式类
  (message "查询所有带 'md:' 前缀的类:")
  (let ((responsive-nodes (ecss-dom-query-tailwind-pattern nav-dom "md:")))
    (message "  找到 %d 个使用响应式类的节点" (length responsive-nodes)))
  
  ;; 查询所有交互状态类
  (message "\n查询所有带 'hover:' 前缀的类:")
  (let ((hover-nodes (ecss-dom-query-tailwind-pattern nav-dom "hover:")))
    (message "  找到 %d 个使用hover状态的节点" (length hover-nodes)))
  
  ;; 动态添加深色模式支持
  (message "\n为链接添加深色模式支持:")
  (let ((links (ecss-dom-query-selector-all nav-dom "a")))
    (dolist (link links)
      (ecss-tailwind-add-class link "dark:text-gray-300")
      (let* ((attrs (dom-attributes link))
             (class-attr (cdr (assq 'class attrs))))
        (message "  更新链接类: %s" class-attr)))))

(message "\n\n=== 所有示例完成 ===\n")

(provide 'example-tailwind)

;;; example-tailwind.el ends here
