# ECSS Tailwind CSS Support

ECSS 现在支持 Tailwind CSS 实用类的解析和操作，以及**转换为原生CSS样式**！

## 功能特性 / Features

- ✅ 解析 Tailwind CSS 类名（包括变体和任意值）
- ✅ 验证 Tailwind 类名的有效性
- ✅ 支持响应式前缀（sm:, md:, lg:, xl:, 2xl:）
- ✅ 支持状态变体（hover:, focus:, active:, 等）
- ✅ 支持任意值语法（如 bg-[#1da1f2]）
- ✅ 在 DOM 树中查询 Tailwind 类
- ✅ 操作 DOM 节点的 Tailwind 类
- ✅ 按属性获取和过滤 Tailwind 类
- ✅ **将 Tailwind 类转换为原生 CSS 样式** 🆕
- ✅ **支持颜色、间距、尺寸、字体等所有常用属性转换** 🆕
- ✅ **直接将 CSS 应用到 DOM 节点** 🆕

## 安装 / Installation

```elisp
(require 'ecss)  ; 这会自动加载 ecss-tailwind
```

## 使用示例 / Usage Examples

### 1. 解析 Tailwind 类名

```elisp
;; 解析简单类
(ecss-tailwind-parse-class "flex")
;; => (:variants nil :utility "flex" :property "flex" :value nil :arbitrary nil)

;; 解析带值的类
(ecss-tailwind-parse-class "bg-red-500")
;; => (:variants nil :utility "bg-red-500" :property "bg" :value "red-500" :arbitrary nil)

;; 解析带变体的类
(ecss-tailwind-parse-class "md:hover:bg-blue-500")
;; => (:variants ("md" "hover") :utility "bg-blue-500" :property "bg" :value "blue-500" :arbitrary nil)

;; 解析任意值
(ecss-tailwind-parse-class "bg-[#1da1f2]")
;; => (:variants nil :utility "bg-[#1da1f2]" :property "bg" :value nil :arbitrary "#1da1f2")
```

### 2. 验证 Tailwind 类名

```elisp
(ecss-tailwind-class-p "flex")                    ; => t
(ecss-tailwind-class-p "bg-red-500")              ; => t
(ecss-tailwind-class-p "md:hover:text-xl")        ; => t
(ecss-tailwind-class-p "not-a-tailwind-class")    ; => nil
```

### 3. 在 DOM 中查询 Tailwind 类

```elisp
;; 创建示例 DOM
(setq my-dom '(div ((class . "flex items-center bg-white"))
                   (span ((class . "text-lg font-bold")) "Hello")
                   (button ((class . "px-4 py-2 bg-blue-500 hover:bg-blue-600")) "Click")))

;; 查询特定类
(ecss-dom-query-tailwind my-dom "flex")
;; => 返回所有具有 "flex" 类的节点

;; 使用模式查询
(ecss-dom-query-tailwind-pattern my-dom "^bg-")
;; => 返回所有以 "bg-" 开头的类的节点

(ecss-dom-query-tailwind-pattern my-dom "^hover:")
;; => 返回所有使用 hover: 变体的节点
```

### 4. 操作 DOM 节点的类

```elisp
(setq node '(div ((class . "flex items-center"))))

;; 添加类
(ecss-tailwind-add-class node "bg-red-500")
;; node 现在有: "flex items-center bg-red-500"

;; 移除类
(ecss-tailwind-remove-class node "flex")
;; node 现在有: "items-center bg-red-500"

;; 替换类
(ecss-tailwind-replace-class node "bg-red-500" "bg-blue-500")
;; node 现在有: "items-center bg-blue-500"

;; 切换类
(ecss-tailwind-toggle-class node "hidden")
;; 添加或移除 "hidden" 类
```

### 5. 按属性获取类

```elisp
(setq node '(div ((class . "bg-red-500 text-lg hover:bg-red-600 p-4 bg-opacity-50"))))

;; 获取所有背景相关的类
(ecss-tailwind-get-classes-by-property node "bg")
;; => ("bg-red-500" "hover:bg-red-600" "bg-opacity-50")

;; 获取所有文本相关的类
(ecss-tailwind-get-classes-by-property node "text")
;; => ("text-lg")
```

### 6. 实用函数

```elisp
;; 获取变体
(ecss-tailwind-get-variants "md:hover:bg-red-500")
;; => ("md" "hover")

;; 获取实用类（不含变体）
(ecss-tailwind-get-utility "md:hover:bg-red-500")
;; => "bg-red-500"

;; 获取属性
(ecss-tailwind-get-property "text-lg")
;; => "text"

;; 检查是否有特定变体
(ecss-tailwind-has-variant-p "md:flex" "md")
;; => t

;; 检查是否有响应式前缀
(ecss-tailwind-has-responsive-p "lg:text-xl")
;; => t

;; 检查是否有状态变体
(ecss-tailwind-has-state-variant-p "hover:bg-blue-500")
;; => t

;; 描述类
(ecss-tailwind-describe-class "md:hover:bg-red-500")
;; => 返回详细的描述字符串
```

### 7. 过滤类

```elisp
(setq classes '("flex" "md:grid" "bg-red-500" "hover:bg-red-600" "not-tailwind"))

;; 过滤出有效的 Tailwind 类
(ecss-tailwind-filter-classes classes)
;; => ("flex" "md:grid" "bg-red-500" "hover:bg-red-600")

;; 使用自定义过滤器
(ecss-tailwind-filter-classes 
  classes
  (lambda (parsed)
    (string-prefix-p "bg" (plist-get parsed :property))))
;; => ("bg-red-500" "hover:bg-red-600")
```

### 8. 将 Tailwind 类转换为原生 CSS 🆕

```elisp
;; 转换单个 Tailwind 类
(ecss-tailwind-to-css "bg-red-500")
;; => ((background-color . "#ef4444"))

(ecss-tailwind-to-css "text-lg")
;; => ((font-size . "1.125rem") (line-height . "1.75rem"))

(ecss-tailwind-to-css "p-4")
;; => ((padding . "1rem"))

(ecss-tailwind-to-css "flex")
;; => ((display . "flex"))

;; 转换多个 Tailwind 类
(ecss-tailwind-classes-to-css "flex items-center bg-blue-500 text-white p-4 rounded")
;; => ((display . "flex") 
;;     (align-items . "center")
;;     (background-color . "#3b82f6")
;;     (color . "#ffffff")
;;     (padding . "1rem")
;;     (border-radius . "0.25rem"))

;; 转换为 CSS 字符串
(ecss-tailwind-css-to-string 
  (ecss-tailwind-classes-to-css "flex items-center bg-blue-500 p-4"))
;; => "display: flex; align-items: center; background-color: #3b82f6; padding: 1rem"

;; 直接应用到 DOM 节点
(ecss-tailwind-apply-css-to-node node "bg-red-500 text-white p-4 rounded-lg")
;; 这会将 Tailwind 类转换为 CSS 并添加到节点的 style 属性
```

## 支持的 Tailwind 特性

### 响应式前缀
- `sm:` - Small screens (640px+)
- `md:` - Medium screens (768px+)
- `lg:` - Large screens (1024px+)
- `xl:` - Extra large screens (1280px+)
- `2xl:` - 2X Extra large screens (1536px+)

### 状态变体
- `hover:` - 鼠标悬停
- `focus:` - 获得焦点
- `active:` - 激活状态
- `visited:` - 已访问
- `disabled:` - 禁用状态
- `checked:` - 选中状态
- 等等...

### 实用类前缀
支持所有常见的 Tailwind 实用类前缀，包括：
- Layout: `flex`, `grid`, `block`, `hidden`, etc.
- Spacing: `p-`, `m-`, `space-`, etc.
- Sizing: `w-`, `h-`, `min-w-`, `max-h-`, etc.
- Typography: `text-`, `font-`, `leading-`, etc.
- Backgrounds: `bg-`, `from-`, `via-`, `to-`, etc.
- Borders: `border-`, `rounded-`, `ring-`, etc.
- Effects: `shadow-`, `opacity-`, `blur-`, etc.
- 以及更多...

### 任意值
支持 Tailwind 的任意值语法：
- `bg-[#1da1f2]` - 自定义颜色
- `text-[14px]` - 自定义大小
- `w-[calc(100%-2rem)]` - 自定义计算值

### CSS 转换支持 🆕
支持将以下 Tailwind 类转换为原生 CSS：

**布局（Layout）**
- Display: `block`, `inline`, `flex`, `grid`, `hidden`, etc.
- Position: `static`, `fixed`, `absolute`, `relative`, `sticky`
- Visibility: `visible`, `invisible`

**颜色（Colors）**
- 背景色: `bg-red-500`, `bg-blue-600`, etc.
- 文字色: `text-gray-900`, `text-white`, etc.
- 边框色: `border-red-500`, etc.
- 支持完整的 Tailwind 颜色调色板（slate, gray, red, orange, yellow, green, blue, indigo, purple, pink）

**间距（Spacing）**
- Padding: `p-4`, `px-8`, `py-2`, `pt-4`, `pr-2`, etc.
- Margin: `m-4`, `mx-auto`, `my-8`, `mt-2`, etc.
- Gap: `gap-4`, `gap-x-2`, etc.
- 完整的间距比例尺（0-96）

**尺寸（Sizing）**
- Width: `w-64`, `w-full`, `w-screen`, `w-1/2`, etc.
- Height: `h-32`, `h-full`, `h-screen`, etc.
- Min/Max: `min-w-0`, `max-h-screen`, etc.

**字体（Typography）**
- 字体大小: `text-xs`, `text-lg`, `text-2xl`, etc.（包含行高）
- 字体粗细: `font-thin`, `font-bold`, `font-black`, etc.
- 文字对齐: `text-left`, `text-center`, `text-right`, etc.

**边框（Borders）**
- 圆角: `rounded`, `rounded-lg`, `rounded-full`, etc.
- 边框宽度: `border`, `border-2`, `border-4`, etc.

**效果（Effects）**
- 阴影: `shadow`, `shadow-md`, `shadow-lg`, etc.
- 透明度: `opacity-50`, `opacity-75`, etc.

**Flexbox**
- Justify: `justify-start`, `justify-center`, `justify-between`, etc.
- Align: `items-start`, `items-center`, `items-stretch`, etc.
- Flex: `flex-1`, `flex-auto`, `flex-none`, etc.

**其他**
- Z-index: `z-0`, `z-10`, `z-50`, etc.

所有转换都遵循 Tailwind CSS 的默认配置和值。


## 实际应用示例

### 构建响应式组件

```elisp
;; 创建一个响应式卡片组件
(setq card '(div ((class . "bg-white rounded-lg shadow-md p-4 md:p-6 lg:p-8"))
                 (h2 ((class . "text-xl md:text-2xl lg:text-3xl font-bold mb-2")) "标题")
                 (p ((class . "text-gray-600 text-sm md:text-base")) "内容...")))

;; 查询所有使用响应式断点的节点
(ecss-dom-query-tailwind-pattern card "^md:")

;; 为深色模式添加类
(dolist (node (ecss-dom-query-selector-all card "div, h2, p"))
  (when (ecss-dom-has-class node "bg-white")
    (ecss-tailwind-add-class node "dark:bg-gray-800"))
  (when (ecss-dom-has-class node "text-gray-600")
    (ecss-tailwind-add-class node "dark:text-gray-300")))
```

### 动态样式切换

```elisp
;; 切换按钮样式
(defun toggle-button-style (button)
  "在主要和次要按钮样式之间切换"
  (if (ecss-dom-has-class button "bg-blue-500")
      (progn
        (ecss-tailwind-replace-class button "bg-blue-500" "bg-gray-200")
        (ecss-tailwind-replace-class button "text-white" "text-gray-800")
        (ecss-tailwind-replace-class button "hover:bg-blue-600" "hover:bg-gray-300"))
    (progn
      (ecss-tailwind-replace-class button "bg-gray-200" "bg-blue-500")
      (ecss-tailwind-replace-class button "text-gray-800" "text-white")
      (ecss-tailwind-replace-class button "hover:bg-gray-300" "hover:bg-blue-600"))))
```

## API 参考

### 解析函数
- `ecss-tailwind-parse-class` - 解析 Tailwind 类名
- `ecss-tailwind-class-p` - 检查类名是否有效

### 查询函数
- `ecss-dom-query-tailwind` - 按类名查询
- `ecss-dom-query-tailwind-pattern` - 按模式查询

### 操作函数
- `ecss-tailwind-add-class` - 添加类
- `ecss-tailwind-remove-class` - 移除类
- `ecss-tailwind-toggle-class` - 切换类
- `ecss-tailwind-replace-class` - 替换类

### 实用函数
- `ecss-tailwind-get-variants` - 获取变体
- `ecss-tailwind-get-utility` - 获取实用类
- `ecss-tailwind-get-property` - 获取属性
- `ecss-tailwind-has-variant-p` - 检查变体
- `ecss-tailwind-has-responsive-p` - 检查响应式前缀
- `ecss-tailwind-has-state-variant-p` - 检查状态变体
- `ecss-tailwind-get-classes-by-property` - 按属性获取类
- `ecss-tailwind-filter-classes` - 过滤类
- `ecss-tailwind-describe-class` - 描述类

### CSS 转换函数 🆕
- `ecss-tailwind-to-css` - 将单个 Tailwind 类转换为 CSS
- `ecss-tailwind-classes-to-css` - 将多个 Tailwind 类转换为 CSS
- `ecss-tailwind-css-to-string` - 将 CSS 属性转换为字符串
- `ecss-tailwind-apply-css-to-node` - 将 Tailwind 类转换为 CSS 并应用到 DOM 节点

## 测试

运行测试：

```bash
emacs --batch -l ecss-selector.el -l ecss-dom.el -l ecss-tailwind.el -l test-tailwind.el -f run-all-tailwind-tests
```

运行示例：

```bash
emacs --batch -l ecss-selector.el -l ecss-dom.el -l ecss-tailwind.el -l example-tailwind.el
```

## 贡献

欢迎提交 issue 和 pull request！

## 许可证

遵循项目的主要许可证（GPLv3）。
