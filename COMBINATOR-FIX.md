# CSS Combinator Bug Fix

## Problem Description

The following CSS combinators were not working correctly in ECSS:

1. **子元素组合器** (Child combinator `>`)
2. **相邻兄弟组合器** (Adjacent sibling combinator `+`)
3. **后续兄弟组合器** (General sibling combinator `~`)

## Root Causes

### 1. Stub Implementations

The adjacent sibling and general sibling combinator matching functions were stub implementations that simply returned `t` (true) for all cases, without actually checking the sibling relationships:

```elisp
(defun ecss-dom-matches-adjacent-sibling-combinator (node prev-sibling-nodes dom)
  ;; 简化实现
  t)

(defun ecss-dom-matches-general-sibling-combinator (node sibling-nodes dom)
  ;; 简化实现
  t)
```

### 2. Missing Rightmost Combinator Usage

The query algorithm had a bug where the rightmost combinator in a selector was not being used. For example, in `div > span`:
- The selector parts were: `[({[div]} . nil), ({[span]} . ">")]`
- The `>` combinator was stored with the `span` part but never actually checked
- Only the combinator from the preceding part (which was `nil`) was used

## Fixes Applied

### 1. Helper Functions for Finding Siblings

Added two helper functions to find sibling nodes in the DOM tree:

```elisp
(defun ecss-dom-get-previous-sibling (node dom)
  "获取节点NODE的前一个兄弟元素节点（跳过文本节点）。")

(defun ecss-dom-get-previous-siblings (node dom)
  "获取节点NODE之前的所有兄弟元素节点（跳过文本节点）。")
```

These functions:
- Walk the DOM tree to find the parent of the given node
- Extract the children of the parent
- Filter for element nodes (skipping text nodes)
- Return the previous sibling(s) in document order

### 2. Proper Adjacent Sibling Combinator Implementation

```elisp
(defun ecss-dom-matches-adjacent-sibling-combinator (node prev-sibling-nodes dom)
  (let ((prev-sibling (ecss-dom-get-previous-sibling node dom)))
    (and prev-sibling
         (ecss-dom-node-matches-selector-part prev-sibling prev-sibling-nodes))))
```

This now:
- Finds the immediate previous sibling of the node
- Checks if that sibling matches the selector
- Returns `t` only if both conditions are met

### 3. Proper General Sibling Combinator Implementation

```elisp
(defun ecss-dom-matches-general-sibling-combinator (node sibling-nodes dom)
  (let ((prev-siblings (ecss-dom-get-previous-siblings node dom))
        (found nil))
    (dolist (sibling prev-siblings)
      (when (ecss-dom-node-matches-selector-part sibling sibling-nodes)
        (setq found t)))
    found))
```

This now:
- Finds all previous siblings of the node
- Checks if any of them match the selector
- Returns `t` if at least one matches

### 4. Fixed Rightmost Combinator Usage

Updated `ecss-dom-query-selector-complex` to extract and pass the rightmost combinator:

```elisp
(let* ((rightmost-part (car (last parts)))
       (preceding-parts (butlast parts))
       (rightmost-combinator (cdr rightmost-part)))  ; Extract combinator
  ...
  (ecss-dom-check-combinator-chain node preceding-parts rightmost-combinator dom))
```

Updated `ecss-dom-check-combinator-chain` to use the rightmost combinator:

```elisp
(defun ecss-dom-check-combinator-chain (node parts rightmost-combinator dom)
  ...
  (combinator (or rightmost-combinator (cdr current-part)))  ; Use rightmost first
  ...)
```

## Examples

See `combinator-examples.el` for comprehensive examples of all combinator types.

### Child Combinator (`>`)

```elisp
;; Select direct children
(ecss-dom-query-selector-all dom "div#container > p")
;; Returns only <p> elements that are direct children of <div#container>
```

### Adjacent Sibling Combinator (`+`)

```elisp
;; Select immediate next sibling
(ecss-dom-query-selector-all dom "p.intro + p")
;; Returns <p> elements immediately following <p.intro>
```

### General Sibling Combinator (`~`)

```elisp
;; Select all following siblings
(ecss-dom-query-selector-all dom "h1 ~ p")
;; Returns all <p> elements that are siblings after <h1>
```

## Testing

To test the fixes, run:

```elisp
(load "combinator-examples.el")
(run-all-combinator-examples)
```

This will execute all example queries and display the results.

## Limitations

- **Multiple combinators**: Selectors with multiple combinators (e.g., `div > p + span`) are not fully supported because the recursive chain checking is simplified
- **Complex chains**: The current implementation works correctly for single combinator selectors but may not handle all complex multi-combinator cases

## Impact

These fixes enable proper CSS selector querying for:
- Direct child relationships (`>`)
- Adjacent sibling relationships (`+`)
- General sibling relationships (`~`)

This brings ECSS closer to full CSS selector compatibility and enables more powerful DOM queries.
