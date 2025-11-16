# CSS Pseudo-class Implementation Summary

## Overview

This implementation adds comprehensive support for common CSS pseudo-classes to the ECSS library. The pseudo-classes are now fully functional and match CSS3 specification behavior.

## Implemented Pseudo-classes

### Structural Position Pseudo-classes

#### `:first-child`
Matches an element if it is the first child (element) of its parent.

**Example:**
```css
p:first-child  /* Matches <p> only if it's the first child */
```

#### `:last-child`
Matches an element if it is the last child (element) of its parent.

**Example:**
```css
li:last-child  /* Matches the last <li> in a list */
```

#### `:only-child`
Matches an element if it is the only child (element) of its parent.

**Example:**
```css
p:only-child  /* Matches <p> only if it has no siblings */
```

### Type-specific Position Pseudo-classes

#### `:first-of-type`
Matches the first element of a specific type among siblings.

**Example:**
```css
p:first-of-type  /* Matches first <p> among siblings, even if not first child */
```

#### `:last-of-type`
Matches the last element of a specific type among siblings.

**Example:**
```css
p:last-of-type  /* Matches last <p> among siblings */
```

#### `:only-of-type`
Matches an element if it's the only one of its type among siblings.

**Example:**
```css
h1:only-of-type  /* Matches <h1> if it's the only <h1> among siblings */
```

### Nth Position Pseudo-classes

#### `:nth-child(an+b)`
Matches elements based on their position among all siblings.

**Supported Expressions:**
- `odd` - matches 1st, 3rd, 5th, etc. (equivalent to `2n+1`)
- `even` - matches 2nd, 4th, 6th, etc. (equivalent to `2n`)
- `3` - matches exactly the 3rd child
- `2n` - matches every 2nd child (2, 4, 6, ...)
- `2n+1` - matches every 2nd child starting from 1st (1, 3, 5, ...)
- `3n+2` - matches every 3rd child starting from 2nd (2, 5, 8, ...)
- `-n+3` - matches first 3 children (1, 2, 3)
- `n+2` - matches 2nd child and after (2, 3, 4, ...)

**Examples:**
```css
li:nth-child(odd)     /* Odd-numbered list items */
li:nth-child(3n+1)    /* Every 3rd item starting from 1st */
p:nth-child(2)        /* Second paragraph */
```

#### `:nth-last-child(an+b)`
Like `:nth-child()` but counts from the end.

**Example:**
```css
li:nth-last-child(1)  /* Same as li:last-child */
li:nth-last-child(2)  /* Second to last */
```

#### `:nth-of-type(an+b)`
Like `:nth-child()` but only counts elements of the same type.

**Example:**
```css
p:nth-of-type(2)      /* Second <p> among sibling <p> elements */
```

#### `:nth-last-of-type(an+b)`
Like `:nth-of-type()` but counts from the end.

**Example:**
```css
p:nth-last-of-type(1) /* Last <p> among sibling <p> elements */
```

### Content-based Pseudo-classes

#### `:empty`
Matches elements that have no children or only whitespace text nodes.

**Example:**
```css
div:empty  /* Matches empty divs */
```

### Negation Pseudo-class

#### `:not(selector)`
Matches elements that do not match the given simple selector.

**Supported in :not():**
- Type selectors: `div`, `p`, etc.
- Class selectors: `.classname`
- ID selectors: `#id`
- Attribute selectors: `[attr]`, `[attr="value"]`
- Other pseudo-classes: `:first-child`, etc.

**Examples:**
```css
p:not(.intro)         /* All <p> except those with class "intro" */
article:not(:first-child)  /* All articles except the first */
```

## Implementation Details

### Architecture

The implementation follows a layered approach:

1. **Parser Layer** (ecss-selector.el)
   - Already supported pseudo-class syntax parsing
   - Parses `:pseudo-class` and `:pseudo-class(expression)` formats

2. **Matching Layer** (ecss-dom.el)
   - New helper functions for DOM traversal
   - Pseudo-class matching logic
   - Nth expression parser

3. **Query Layer** (ecss-dom.el)
   - Integration with existing selector query system
   - Dynamic variable `ecss-dom--query-root` for parent lookups

### Key Functions

#### Helper Functions
```elisp
(ecss-dom-get-parent node dom)
;; Returns the parent node of the given node

(ecss-dom-get-element-children node)
;; Returns element children (excludes text nodes)

(ecss-dom-get-child-index node)
;; Returns 1-based index of node among siblings
```

#### Matching Functions
```elisp
(ecss-dom-is-first-child node)
(ecss-dom-is-last-child node)
(ecss-dom-is-first-of-type node)
(ecss-dom-is-last-of-type node)
(ecss-dom-is-only-of-type node)
(ecss-dom-is-empty node)
```

#### Nth Expression Parser
```elisp
(ecss-dom-parse-nth-expression "2n+1")
;; => (2 . 1)  ; Represents an+b form

(ecss-dom-matches-nth index a b)
;; Returns t if index matches the an+b pattern
```

### Query Root Management

The implementation uses a dynamic variable `ecss-dom--query-root` to track the DOM root during queries. This allows pseudo-class matching functions to traverse the DOM tree to find parent and sibling relationships.

```elisp
(let ((ecss-dom--query-root dom))
  ;; Pseudo-class functions can now access the full DOM tree
  (ecss-dom-query-selector-all dom "p:first-child"))
```

## Testing

### Test Coverage

The implementation includes comprehensive tests in `test-pseudo-classes.el`:

1. **Nth Expression Parsing** - 6 test cases
2. **:first-child** - 3 test cases
3. **:last-child** - 3 test cases
4. **:only-child** - 2 test cases
5. **:first-of-type** - 2 test cases
6. **:last-of-type** - 2 test cases
7. **:only-of-type** - 2 test cases
8. **:empty** - 3 test cases
9. **:nth-child()** - 4 test cases
10. **:nth-last-child()** - 2 test cases
11. **:nth-of-type()** - 2 test cases
12. **:nth-last-of-type()** - 2 test cases
13. **:not()** - 2 test cases
14. **Combined pseudo-classes** - 1 test case

**Total: 36 test cases, all passing**

### Running Tests

```bash
# Run pseudo-class tests
emacs --batch -L . -l ecss-selector.el -l ecss-dom.el \
  -l test-pseudo-classes.el -f run-all-pseudo-class-tests

# Run existing tests (still passing)
emacs --batch -L . -l ecss-selector.el \
  -l test-selector-attribute.el -f run-attribute-selector-tests
```

## Examples

### Basic Usage

```elisp
(require 'ecss-dom)

(let ((dom '(ul nil
                (li nil "First")
                (li nil "Second")
                (li nil "Third"))))
  
  ;; Select first item
  (ecss-dom-query-selector-all dom "li:first-child")
  ;; => ((li nil "First"))
  
  ;; Select last item
  (ecss-dom-query-selector-all dom "li:last-child")
  ;; => ((li nil "Third"))
  
  ;; Select odd items
  (ecss-dom-query-selector-all dom "li:nth-child(odd)")
  ;; => ((li nil "First") (li nil "Third"))
  )
```

### Styling with Pseudo-classes

```elisp
;; Style first paragraph in each article
(ecss-dom-apply-style 
  dom 
  "article p:first-of-type"
  '((font-weight . "bold")))

;; Zebra striping for list items
(ecss-dom-apply-style
  dom
  "li:nth-child(odd)"
  '((background-color . "#f5f5f5")))

;; Remove border from last item
(ecss-dom-apply-style
  dom
  "li:last-child"
  '((border-bottom . "none")))
```

### Practical Blog Example

See `example-pseudo-classes.el` for comprehensive real-world examples including:
- Blog post styling
- List item striping
- Featured content highlighting
- Navigation styling

## Compatibility

### CSS3 Compatibility

The implementation follows CSS3 Selectors Level 3 specification for:
- Structural pseudo-classes
- Nth-child expressions
- :not() negation (simple selectors only)

### Limitations

1. **:not() complexity**: Only supports simple selectors, not compound selectors
   - ✅ Supported: `:not(.class)`, `:not(#id)`, `:not([attr])`
   - ❌ Not supported: `:not(div.class)`, `:not(div > p)`

2. **Text nodes**: Text nodes are excluded from child/sibling counting, matching browser behavior

3. **Performance**: Parent/sibling lookups require DOM tree traversal. Acceptable for typical DOM sizes but may be slower for very large trees.

## Future Enhancements

Potential additions (not in current scope):
- `:nth-child(an+b of selector)` - CSS Selectors Level 4
- `:is()` and `:where()` pseudo-classes
- `:has()` relational pseudo-class
- `:focus`, `:hover`, `:active` - UI state pseudo-classes
- `:checked`, `:disabled` - form state pseudo-classes

## Conclusion

This implementation provides comprehensive, spec-compliant support for the most commonly used CSS pseudo-classes. The code is well-tested, documented, and includes practical examples. The implementation integrates seamlessly with the existing ECSS selector system.
