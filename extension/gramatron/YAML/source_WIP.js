{
  "b-as-line-feed": ["b-break"],
  "b-as-space": ["b-break"],
  "b-break": ["b-carriage-return b-line-feed", "b-carriage-return", "b-line-feed"],
  "b-carriage-return": ["'#xD'"],
  "b-char": ["b-line-feed", "b-carriage-return"],
  "b-chomped-last(t)": [
    t = "strip" -> "b-non-content", EOF
    t = "clip"  -> "b-as-line-feed", EOF
    t = "keep"  -> "b-as-line-feed", EOF 
  ],
  "b-comment": ["b-non-content", EOF],
  "b-l-folded(n,c)": ["b-l-trimmed(n,c)", "b-as-space"],
  "b-l-spaced(n)": ["b-as-line-feed l-empty(n,block-in)*"],
  "b-l-trimmed(n,c)": ["b-non-content l-empty(n,c)+"],
  "b-line-feed": ["'#xA'"],
  "b-nb-literal-next(n)": ["b-as-line-feed l-nb-literal-text(n)"],
  "b-non-content": ["b-break"],
  "c-alias": ["'*'"],
  "c-anchor": ["'&'"],
  "c-b-block-header(m,t)": [
    "c-indentation-indicator(m) c-chomping-indicator(t) s-b-comment",
    "c-chomping-indicator(t) c-indentation-indicator(m) s-b-comment"
  ],
  "c-byte-order-mark": ["'#xFEFF'"],
  "c-chomping-indicator(t)": [
    "'-'"         -> t = "strip"
    "'+'"         -> t = "keep"
    EMPTY -> t = "clip"
  ],
  "c-collect-entry": ["','"],
  "c-comment": ["'#'"],
  "c-directive": ["'%'"],
  "c-directives-end": ["'---'"],
  "c-document-end": ["'...'"],
  "c-double-quote": ["'\""],
  "c-double-quoted(n,c)": ["'\"' nb-double-text(n,c) '\"'"],
  "c-escape": ["'\\'"],
  "c-flow-indicator": ["','", "'['", "']'", "'{'", "'}'"],
  "c-flow-json-content(n,c)": ["c-flow-sequence(n,c)", "c-flow-mapping(n,c)", "c-single-quoted(n,c)", "c-double-quoted(n,c)"],
  "c-flow-json-node(n,c)": [
    "c-ns-properties(n,c) c-flow-json-content(n,c)",
    "c-ns-properties(n,c) s-separate(n,c) c-flow-json-content(n,c)"
  ],
  "c-flow-mapping(n,c)": [
    "'{}'",
    "'{' s-separate(n,c) '}'",
    "'{' ns-s-flow-map-entries(n,in-flow(c)) '}'",
    "'{' s-separate(n,c) ns-s-flow-map-entries(n,in-flow(c)) '}'"
  ],
  "c-flow-sequence(n,c)": [
    "'[]'"
    "'[' s-separate(n,c) ']'"
    "'[' ns-s-flow-seq-entries(n,in-flow(c)) ']'"
    "'[' s-separate(n,c) ns-s-flow-seq-entries(n,in-flow(c)) ']'"
  ],
  "c-folded": ["'>'"],
  "c-forbidden": [
    SOL "c-directives-end b-char",
    SOL "c-directives-end s-white",
    SOL "c-directives-end" EOF,
    SOL "c-document-end b-char",
    SOL "c-document-end s-white",
    SOL "c-document-end" EOF
  ],
  "c-indentation-indicator(m)": [
    "ns-dec-digit" -> m = "ns-dec-digit" - "'#x30'"
    EMPTY  -> m = "auto-detect()"
  ],
  "c-indicator": ["'-'", "'?'", "':'", "','", "'['", "']'", "'{'", "'}'", "'#'", "'&'", "'*'", "'!'", "'|'", "'>'", "'''", "'\"'", "'%'", "'@'", "'`'"],
  "c-l+folded(n)": ["'>' c-b-block-header(m,t) l-folded-content(n+m,t)"],
  "c-l+literal(n)": ["'|' c-b-block-header(m,t) l-literal-content(n+m,t)"],
  "c-l-block-map-explicit-entry(n)": [
    "c-l-block-map-explicit-key(n) l-block-map-explicit-value(n)",
    "c-l-block-map-explicit-key(n) e-node"
  ],
  "c-l-block-map-explicit-key(n)": ["'?' s-l+block-indented(n,block-out)"],
  "c-l-block-map-implicit-value(n)": [
    "':' e-node s-l-comments",
    "':' s-l+block-node(n,block-out)"
  ],
  "c-l-block-seq-entry(n)": ["'-' s-l+block-indented(n,block-in)"],
  "c-literal": ["'|'"],
  "c-mapping-end": ["'}'"],
  "c-mapping-key": ["'?'"],
  "c-mapping-start": ["'{'"],
  "c-mapping-value": ["':'"],
  "c-named-tag-handle": ["'!' ns-word-char+ !"],
  "c-nb-comment-text": ["'#' nb-char*"],
  "c-non-specific-tag": ["'!'"],
  "c-ns-alias-node": ["'*' ns-anchor-name"],
  "c-ns-anchor-property": ["'&' ns-anchor-name"],
  "c-ns-esc-char": [
    "'\\' ns-esc-null", "'\\' ns-esc-bell", "'\\' ns-esc-backspace", "'\\' ns-esc-horizontal-tab", "'\\' ns-esc-line-feed",
    "'\\' ns-esc-vertical-tab", "'\\' ns-esc-form-feed", "'\\' ns-esc-carriage-return", "'\\' ns-esc-escape", "'\\' ns-esc-space",
    "'\\' ns-esc-double-quote", "'\\' ns-esc-slash", "'\\' ns-esc-backslash", "'\\' ns-esc-next-line", "'\\' ns-esc-non-breaking-space",
    "'\\' ns-esc-line-separator", "'\\' ns-esc-paragraph-separator", "'\\' ns-esc-8-bit", "'\\' ns-esc-16-bit", "'\\' ns-esc-32-bit"
  ],
  "c-ns-flow-map-adjacent-value(n,c)": [
    "':' e-node",
    "':' ns-flow-node(n,c)",
    "':' s-separate(n,c) ns-flow-node(n,c)"
  ],
  "c-ns-flow-map-empty-key-entry(n,c)": ["e-node c-ns-flow-map-separate-value(n,c)"],
  "c-ns-flow-map-json-key-entry(n,c)": [
    "c-flow-json-node(n,c) e-node",
    "c-flow-json-node(n,c) c-ns-flow-map-adjacent-value(n,c)",
    "c-flow-json-node(n,c) s-separate(n,c) c-ns-flow-map-adjacent-value(n,c)"
  ],
  "c-ns-flow-map-separate-value(n,c)": [
    "':' e-node", 
    "':' s-separate(n,c) ns-flow-node(n,c)"
  ],
  "c-ns-flow-pair-json-key-entry(n,c)": ["c-s-implicit-json-key(flow-key) c-ns-flow-map-adjacent-value(n,c)"],
  "c-ns-local-tag-prefix": ["'!' ns-uri-char*"],
  "c-ns-properties(n,c)": [
    "c-ns-tag-property", 
    "c-ns-tag-property s-separate(n,c) c-ns-anchor-property", 
    "c-ns-anchor-property",
    "c-ns-anchor-property s-separate(n,c) c-ns-tag-property",
  ],
  "c-ns-shorthand-tag": ["c-tag-handle ns-tag-char+"],
  "c-ns-tag-property": ["c-verbatim-tag", "c-ns-shorthand-tag", "c-non-specific-tag"],
  "c-primary-tag-handle": ["'!'"],
  "c-printable": ["'#x9'", "'#xA'", "'#xD'", "'#x20'-'#x7E'"/*8bit*/, "'#x85'", "'#xA0'-'#xD7FF'", "'#xE000'-'#xFFFD'"/*16bit*/, "'#x10000'-'#x10FFFF'"/*32bit*/],
  "c-quoted-quote": ["'\'\''"],
  "c-reserved": ["'@'", "'`'"],
  "c-s-implicit-json-key(c)": [
    "c-flow-json-node(0,c)",
    "c-flow-json-node(0,c) s-separate-in-line"
  ],
  "c-secondary-tag-handle": ["'!!'"],
  "c-sequence-end": ["']'"],
  "c-sequence-entry": ["'-'"],
  "c-sequence-start": ["'['"],
  "c-single-quote": ["'\''"],
  "c-single-quoted(n,c)": [
    "'\'' nb-single-multi-line(n) '\''",
    "'\'' nb-single-one-line '\''"
  ],
  "c-tag": ["'!'"],
  "c-tag-handle": ["c-named-tag-handle", "c-secondary-tag-handle", "c-primary-tag-handle"],
  "c-verbatim-tag": ["'!' '<' ns-uri-char+ >"],
  "e-node": ["e-scalar"],
  "e-scalar": [EMPTY],
  "in-flow(c)": [
    c = "flow-out"  -> "flow-in"
    c = "flow-in"   -> "flow-in"
    c = "block-key" -> "flow-key"
    c = "flow-key"  -> "flow-key"
  ],
  "l+block-mapping(n)": ["s-indent(n+m) ns-l-block-map-entry(n+m)"+ /* "For some fixed auto-detected" m > 0 */ ],
  "l+block-sequence(n)": ["s-indent(n+m) c-l-block-seq-entry(n+m)"+ /* "For some fixed auto-detected" m > 0 */ ],
  "l-any-document": ["l-directive-document", "l-explicit-document", "l-bare-document"],
  "l-bare-document": ["s-l+block-node(-1,block-in)"/* "Excluding c-forbidden content" */ ],
  "l-block-map-explicit-value(n)": ["s-indent(n) ':' s-l+block-indented(n,block-out)"],
  "l-chomped-empty(n,t)": [
    t = "strip" -> "l-strip-empty(n)"
    t = "clip"  -> "l-strip-empty(n)"
    t = "keep"  -> "l-keep-empty(n)"],
  "l-comment": ["s-separate-in-line c-nb-comment-text"? "b-comment"],
  "l-directive": [
    "'%' ns-yaml-directive s-l-comments",
    "'%' ns-tag-directive s-l-comments",
    "'%' ns-reserved-directive s-l-comments"
  ],
  "l-directive-document": ["l-directive+ l-explicit-document"],
  "l-document-prefix": ["c-byte-order-mark"? "l-comment*"],
  "l-document-suffix": ["c-document-end s-l-comments"],
  "l-empty(n,c)": [
    "s-line-prefix(n,c) b-as-line-feed",
    "s-indent(<n) b-as-line-feed"
  ],
  "l-explicit-document": [
    "c-directives-end e-node s-l-comments",
    "c-directives-end l-bare-document"
  ],
  "l-folded-content(n,t)": [
    "l-chomped-empty(n,t)",
    "l-nb-diff-lines(n) b-chomped-last(t) l-chomped-empty(n,t)",
  ],
  "l-keep-empty(n)": ["l-empty(n,block-in)* l-trail-comments(n)"? ],
  "l-literal-content(n,t)": [
    "l-chomped-empty(n,t)",
    "l-nb-literal-text(n) b-chomped-last(t) l-chomped-empty(n,t)",
    "l-nb-literal-text(n) b-nb-literal-next(n)+ b-chomped-last(t) l-chomped-empty(n,t)"
  ],
  "l-nb-diff-lines(n)": [
    "l-nb-same-lines(n)",
    "l-nb-same-lines(n)" ("b-as-line-feed l-nb-same-lines(n)")+,
  ],
  "l-nb-folded-lines(n)": [
    "s-nb-folded-text(n)",
    "s-nb-folded-text(n)" ("b-l-folded(n,block-in) s-nb-folded-text(n)")+,
  ],
  "l-nb-literal-text(n)": [
    "s-indent(n) nb-char+",
    "l-empty(n,block-in)+ s-indent(n) nb-char+"
  ],
  "l-nb-same-lines(n)": [
    "l-nb-folded-lines(n)",
    "l-nb-spaced-lines(n)",
    "l-empty(n,block-in)* l-nb-folded-lines(n)",
    "l-empty(n,block-in)* l-nb-spaced-lines(n)"
  ],
  "l-nb-spaced-lines(n)": [
    "s-nb-spaced-text(n)",
    "s-nb-spaced-text(n)" ("b-l-spaced(n) s-nb-spaced-text(n)")* 
  ],
  "l-strip-empty(n)": [
    "l-trail-comments(n)",
    ("s-indent(<=n) b-non-content")+,
    ("s-indent(<=n) b-non-content")+ "l-trail-comments(n)",
  ],
  "l-trail-comments(n)": [
    "s-indent("'<'"n) c-nb-comment-text b-comment",
    "s-indent("'<'"n) c-nb-comment-text b-comment l-comment+",
  ],
  "l-yaml-stream": [
    "l-document-prefix* l-any-document",
    "l-document-prefix"+,
    ("l-document-prefix l-explicit-document")+,
    ("l-document-prefix+ l-explicit-document")+,
    "l-document-suffix+",
    ("l-document-suffix+ l-document-prefix+")+,
    ("l-document-suffix+ l-document-prefix l-any-document")+,
    ("l-document-suffix+ l-document-prefix+ l-any-document")+,
    "l-document-prefix l-any-document l-document-prefix+",
    "l-document-prefix l-any-document" ("l-document-prefix l-explicit-document")+,
    "l-document-prefix l-any-document l-document-prefix+",
    "l-document-prefix l-any-document" ("l-document-prefix+ l-explicit-document")+,
    "l-document-prefix l-any-document l-document-suffix+",
    "l-document-prefix l-any-document" ("l-document-suffix+ l-any-document")+,
    "l-document-prefix l-any-document" ("l-document-suffix+ l-document-prefix+")+,
    "l-document-prefix l-any-document" ("l-document-suffix+ l-document-prefix+ l-any-document")+,
    "l-document-prefix+ l-any-document l-document-prefix+",
    "l-document-prefix+ l-any-document" ("l-document-prefix l-explicit-document")+,
    "l-document-prefix+ l-any-document l-document-prefix+",
    "l-document-prefix+ l-any-document" ("l-document-prefix+ l-explicit-document")+,
    "l-document-prefix+ l-any-document" ("l-document-suffix+ l-document-prefix")+,
    "l-document-prefix+ l-any-document" ("l-document-suffix+ l-document-prefix l-any-document")+,
    "l-document-prefix+ l-any-document" ("l-document-suffix+ l-document-prefix+")+,
    "l-document-prefix+ l-any-document" ("l-document-suffix+ l-document-prefix+ l-any-document")+,
  ],
  "nb-char": ["c-printable" - "b-char" - "c-byte-order-mark"],
  "nb-double-char": [
    "c-ns-esc-char", 
    ( "nb-json" - "\\" - "'\"'"  )
  ],
  "nb-double-multi-line(n)": [
    "nb-ns-double-in-line s-white",
    "nb-ns-double-in-line s-white+",
    "nb-ns-double-in-line s-double-next-line(n)",
  ],
  "nb-double-one-line": ["nb-double-char*"],
  "nb-double-text(n,c)": [
    c = "flow-out"  -> "nb-double-multi-line(n)"
    c = "flow-in"   -> "nb-double-multi-line(n)"
    c = "block-key" -> "nb-double-one-line"
    c = "flow-key"  -> "nb-double-one-line"],
  "nb-json": ["'#x9'", ["'#x20'-'#x10FFFF'"] ],
  "nb-ns-double-in-line": [
    "ns-double-char+",
    ("s-white+ ns-double-char")+,
  ],
  "nb-ns-plain-in-line(c)": [
    ("s-white ns-plain-char(c)")+,
    ("s-white+ ns-plain-char(c)")+,
  ],
  "nb-ns-single-in-line": [
    ("ns-single-char")+,
    ("s-white+ ns-single-char")+,
  ],
  "nb-single-char": [
    "c-quoted-quote", 
    ( "nb-json" - "'''" )
  ],
  "nb-single-multi-line(n)": [
    "nb-ns-single-in-line",
    "nb-ns-single-in-line s-white+",
    "nb-ns-single-in-line s-single-next-line(n)",
  ],
  "nb-single-one-line": ["''", "nb-single-char+"],
  "ns-anchor-char": ["ns-char" - "c-flow-indicator"],
  "ns-anchor-name": ["ns-anchor-char+"],
  "ns-ascii-letter": ["'#x41'-'#x5A'" /* A-Z */, "'#x61'-'#x7A'"/* a-z */ ],
  "ns-char": ["nb-char" - "s-white"],
  "ns-dec-digit": ["'#x30'-'#x39'"/* 0-9 */],
  "ns-directive-name": ["ns-char+"],
  "ns-directive-parameter": ["ns-char+"],
  "ns-double-char": ["nb-double-char" - "s-white"],
  "ns-esc-16-bit": ["'u' ns-hex-digit ns-hex-digit ns-hex-digit ns-hex-digit"],
  "ns-esc-32-bit": ["'U' ns-hex-digit ns-hex-digit ns-hex-digit ns-hex-digit ns-hex-digit ns-hex-digit ns-hex-digit ns-hex-digit"],
  "ns-esc-8-bit": ["'x' ns-hex-digit ns-hex-digit"],
  "ns-esc-backslash": ["\\"],
  "ns-esc-backspace": ["'b'"],
  "ns-esc-bell": ["'a'"],
  "ns-esc-carriage-return": ["'r'"],
  "ns-esc-double-quote": ["'\"'"],
  "ns-esc-escape": ["'e'"],
  "ns-esc-form-feed": ["'f'"],
  "ns-esc-horizontal-tab": ["'t'", "'#x9'"],
  "ns-esc-line-feed": ["'n'"],
  "ns-esc-line-separator": ["'L'"],
  "ns-esc-next-line": ["'N'"],
  "ns-esc-non-breaking-space": ["'_'"],
  "ns-esc-null": ["'0'"],
  "ns-esc-paragraph-separator": ["'P'"],
  "ns-esc-slash": ["'/'"],
  "ns-esc-space": ["'#x20'"],
  "ns-esc-vertical-tab": ["'v'"],
  "ns-flow-content(n,c)": ["ns-flow-yaml-content(n,c)", "c-flow-json-content(n,c)"],
  "ns-flow-map-entry(n,c)": ["'?' s-separate(n,c) ns-flow-map-explicit-entry(n,c)", "ns-flow-map-implicit-entry(n,c)"],
  "ns-flow-map-explicit-entry(n,c)": ["ns-flow-map-implicit-entry(n,c)", "e-node e-node"],
  "ns-flow-map-implicit-entry(n,c)": ["ns-flow-map-yaml-key-entry(n,c)", "c-ns-flow-map-empty-key-entry(n,c)", "c-ns-flow-map-json-key-entry(n,c)"],
  "ns-flow-map-yaml-key-entry(n,c)": [
    "ns-flow-yaml-node(n,c) e-node",
    "ns-flow-yaml-node(n,c) c-ns-flow-map-separate-value(n,c)",
    "ns-flow-yaml-node(n,c) s-separate(n,c) c-ns-flow-map-separate-value(n,c)",
  ],
  "ns-flow-node(n,c)": [
    "c-ns-alias-node", 
    "ns-flow-content(n,c)", 
    "c-ns-properties(n,c) e-scalar",
    "c-ns-properties(n,c) s-separate(n,c) ns-flow-content(n,c)"
  ],
  "ns-flow-pair(n,c)": ["'?' s-separate(n,c) ns-flow-map-explicit-entry(n,c)", "ns-flow-pair-entry(n,c)"],
  "ns-flow-pair-entry(n,c)": ["ns-flow-pair-yaml-key-entry(n,c)", "c-ns-flow-map-empty-key-entry(n,c)", "c-ns-flow-pair-json-key-entry(n,c)"],
  "ns-flow-pair-yaml-key-entry(n,c)": ["ns-s-implicit-yaml-key(flow-key)""c-ns-flow-map-separate-value(n,c)"],
  "ns-flow-seq-entry(n,c)": ["ns-flow-pair(n,c)", "ns-flow-node(n,c)"],
  "ns-flow-yaml-content(n,c)": ["ns-plain(n,c)"],
  "ns-flow-yaml-node(n,c)": [
    "c-ns-alias-node", 
    "ns-flow-yaml-content(n,c)", 
    "c-ns-properties(n,c) e-scalar",
    "c-ns-properties(n,c) s-separate(n,c) ns-flow-yaml-content(n,c)",
  ,
  "ns-global-tag-prefix": [
    "ns-tag-char",
    "ns-tag-char ns-uri-char+",
  ],
  "ns-hex-digit": ["ns-dec-digit", "'#x41'-'#x46'"/*A-F*/, "'#x61'-'#x66'"/*a-f*/],
  "ns-l-block-map-entry(n)": ["c-l-block-map-explicit-entry(n)", "ns-l-block-map-implicit-entry(n)"],
  "ns-l-block-map-implicit-entry(n)": [
    "e-node c-l-block-map-implicit-value(n)",
    "ns-s-block-map-implicit-key c-l-block-map-implicit-value(n)",
  ],
  "ns-l-compact-mapping(n)": [
    "ns-l-block-map-entry(n)",
    "ns-l-block-map-entry(n)" ("s-indent(n) ns-l-block-map-entry(n)")+
  ],
  "ns-l-compact-sequence(n)": [
    "c-l-block-seq-entry(n)",
    "c-l-block-seq-entry(n)" ("s-indent(n) c-l-block-seq-entry(n)")+
  ],
  "ns-plain(n,c)": [
    c = "flow-out"  -> "ns-plain-multi-line(n,c)"
    c = "flow-in"   -> "ns-plain-multi-line(n,c)"
    c = "block-key" -> "ns-plain-one-line(c)"
    c = "flow-key"  -> "ns-plain-one-line(c)"],
  "ns-plain-char(c)": [
    ("ns-plain-safe-out" - "':'" - "'#'"), 
    ("ns-plain-safe-in" - "':'" - "'#'"), 
    ("ns-plain-safe-out" - "':'" - "'#'"), 
    ("ns-plain-safe-in" - "':'" - "'#'"), 
    "'#'",
    "':'"
  ],
  "ns-plain-first(c)": [
    ("ns-char" - "c-indicator"),
    "'?'", /* Followed by an ns-plain-safe(c)) */
    "':'", /* Followed by an ns-plain-safe(c)) */
    "'-'" /* Followed by an ns-plain-safe(c)) */
  ],
  "ns-plain-multi-line(n,c)": [
    "ns-plain-one-line(c)",
    "ns-plain-one-line(c) s-ns-plain-next-line(n,c)+",
  ],
  "ns-plain-one-line(c)": ["ns-plain-first(c) nb-ns-plain-in-line(c)"],
  "ns-plain-safe-in": ["ns-char" - "c-flow-indicator"],
  "ns-plain-safe-out": ["ns-char"],
  "ns-reserved-directive": [
    "ns-directive-name",
    "ns-directive-name" ("s-separate-in-line ns-directive-parameter")+,
  ],
  "ns-s-block-map-implicit-key": ["c-s-implicit-json-key(block-key)", "ns-s-implicit-yaml-key(block-key)"],
  "ns-s-flow-map-entries(n,c)": [
    "',' s-separate(n,c)",
    "ns-s-flow-map-entries(n,c)",
    "ns-flow-map-entry(n,c) s-separate(n,c)",
    "',' s-separate(n,c) ns-s-flow-map-entries(n,c)",
    "ns-flow-map-entry(n,c) s-separate(n,c) ',' s-separate(n,c)",
    "ns-flow-map-entry(n,c) s-separate(n,c) ',' ns-s-flow-map-entries(n,c)",
    "ns-flow-map-entry(n,c) s-separate(n,c) ',' s-separate(n,c) ns-s-flow-map-entries(n,c)",
  ],
  "ns-s-flow-seq-entries(n,c)": [
    "ns-flow-seq-entry(n,c) s-separate(n,c)",
    "ns-flow-seq-entry(n,c) ',' s-separate(n,c)",
    "ns-flow-seq-entry(n,c) ns-s-flow-seq-entries(n,c)",
    "ns-flow-seq-entry(n,c) ',' s-separate(n,c) ns-s-flow-seq-entries(n,c)",
    "ns-flow-seq-entry(n,c) s-separate(n,c) ',' s-separate(n,c)",
    "ns-flow-seq-entry(n,c) s-separate(n,c) ns-s-flow-seq-entries(n,c)",
    "ns-flow-seq-entry(n,c) s-separate(n,c) ',' s-separate(n,c) ns-s-flow-seq-entries(n,c)"
  ],
  "ns-s-implicit-yaml-key(c)": [
    "ns-flow-yaml-node(0,c)",
    "ns-flow-yaml-node(0,c) s-separate-in-line",
  ],
  "ns-single-char": ["nb-single-char" - "s-white"],
  "ns-tag-char": ["ns-uri-char" - "'!'" - "c-flow-indicator"],
  "ns-tag-directive": ["'TAG' s-separate-in-line c-tag-handle s-separate-in-line ns-tag-prefix"],
  "ns-tag-prefix": ["c-ns-local-tag-prefix", "ns-global-tag-prefix"],
  "ns-uri-char": ["'%' ns-hex-digit ns-hex-digit", "ns-word-char", "'#'", "';'", "'/'", "'?'", "':'", "'@'", "'&'", "'='", "'+'", "'$'", "','", "_", "'.'", "'!'", "'~'", "'*'", "'''", "'('", "')'", "'['", "']'"],
  "ns-word-char": ["ns-dec-digit", "ns-ascii-letter", "'-'"],
  "ns-yaml-directive": ["'YAML' s-separate-in-line ns-yaml-version"],
  "ns-yaml-version": ["ns-dec-digit+ '.' ns-dec-digit+"],
  "s-b-comment": [
    "b-comment",
    "s-separate-in-line b-comment",
    "s-separate-in-line c-nb-comment-text b-comment",
  ],
  "s-block-line-prefix(n)": ["s-indent(n)"],
  "s-double-break(n)": ["s-double-escaped(n)", "s-flow-folded(n)"],
  "s-double-escaped(n)": [
    "s-white* '\\' b-non-content s-flow-line-prefix(n)",
    "s-white* '\\' b-non-content l-empty(n,flow-in)+ s-flow-line-prefix(n)",
  ],
  "s-double-next-line(n)": [
    "s-double-break(n)",
    "s-double-break(n) ns-double-char nb-ns-double-in-line",
    "s-double-break(n) ns-double-char nb-ns-double-in-line s-white+",
    "s-double-break(n) ns-double-char nb-ns-double-in-line s-double-next-line(n)",
  ],
  "s-flow-folded(n)": [
    "b-l-folded(n,flow-in) s-flow-line-prefix(n)",
    "s-separate-in-line b-l-folded(n,flow-in) s-flow-line-prefix(n)",
  ],
  "s-flow-line-prefix(n)": [
    "s-indent(n)",
    "s-indent(n) s-separate-in-line",
  ],
  "s-indent(<n)": ["s-space"{m} /* "Where" m < n */ ],
  "s-indent(n)": ["s-space"{n} ],
  "s-indent(<=n)": ["s-space"{m} /* "Where" m <= n */ ],
  "s-l+block-collection(n,c)": [
    "s-l-comments l+block-mapping(n)",
    "s-l-comments l+block-sequence(n-1)",
    "s-l-comments l+block-sequence(n)",
    "s-separate(n+1,c) c-ns-properties(n+1,c) s-l-comments l+block-mapping(n)",
    "s-separate(n+1,c) c-ns-properties(n+1,c) s-l-comments l+block-sequence(n-1)"
    "s-separate(n+1,c) c-ns-properties(n+1,c) s-l-comments l+block-sequence(n)"
  ],
  "s-l+block-in-block(n,c)": ["s-l+block-scalar(n,c)", "s-l+block-collection(n,c)"],
  "s-l+block-indented(n,c)": [
    "s-indent(m) ns-l-compact-mapping(n+1+m)",
    "s-indent(m) ns-l-compact-sequence(n+1+m)",
    "s-l+block-node(n,c)",
    "e-node s-l-comments"
  ],
  "s-l+block-node(n,c)": ["s-l+block-in-block(n,c)", "s-l+flow-in-block(n)"],
  "s-l+block-scalar(n,c)": [
  "s-separate(n+1,c) c-l+folded(n)",
  "s-separate(n+1,c) c-l+literal(n)",
  "s-separate(n+1,c) c-ns-properties(n+1,c) s-separate(n+1,c) c-l+folded(n)",
  "s-separate(n+1,c) c-ns-properties(n+1,c) s-separate(n+1,c) c-l+literal(n)",
  ],
  "s-l+flow-in-block(n)": ["s-separate(n+1,flow-out) ns-flow-node(n+1,flow-out) s-l-comments"],
  "s-l-comments": [
    SOL,
    "s-b-comment",
    SOL "l-comment+",
    "s-b-comment l-comment+",
  ],
  "s-line-prefix(n,c)": [
    c = "block-out" -> "s-block-line-prefix(n)"
    c = "block-in"  -> "s-block-line-prefix(n)"
    c = "flow-out"  -> "s-flow-line-prefix(n)"
    c = "flow-in"   -> "s-flow-line-prefix(n)"
  ],
  "s-nb-folded-text(n)": ["s-indent(n) ns-char nb-char*"],
  "s-nb-spaced-text(n)": ["s-indent(n) s-white nb-char*"],
  "s-ns-plain-next-line(n,c)": ["s-flow-folded(n) ns-plain-char(c) nb-ns-plain-in-line(c)"],
  "s-separate(n,c)": [
    c = "block-out" -> "s-separate-lines(n)"
    c = "block-in"  -> "s-separate-lines(n)"
    c = "flow-out"  -> "s-separate-lines(n)"
    c = "flow-in"   -> "s-separate-lines(n)"
    c = "block-key" -> "s-separate-in-line"
    c = "flow-key"  -> "s-separate-in-line"
  ],
  "s-separate-in-line": ["s-white+", SOL ],
  "s-separate-lines(n)": ["s-l-comments s-flow-line-prefix(n)", "s-separate-in-line"],
  "s-single-next-line(n)": [
    "s-flow-folded(n)",
    "s-flow-folded(n) ns-single-char nb-ns-single-in-line",
    "s-flow-folded(n) ns-single-char nb-ns-single-in-line s-white+",
    "s-flow-folded(n) ns-single-char nb-ns-single-in-line s-single-next-line(n)",
  ],
  "s-space": ["'#x20'"],
  "s-tab": ["'#x9'"],
  "s-white": ["s-space", "s-tab"]
}