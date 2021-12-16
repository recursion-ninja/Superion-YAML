BLANK_LINE: (SPACE | TAB)* NEWLINE;
NEWLINE: '\r'? '\n' | '\r';
...
document: block+ BLANK_LINE* EOF?;
block: BLANK_LINE*
       (htmlBlockTags
       | htmlBlockSelfClosing
       | htmlComment
       | heading
       | horizontalRule
       | blockQuote[0]
       | references
       | {followListItem(1, 0)}? orderedList[0]
       | {followListItem(1, 0)}? bulletList[0]
       | {followVerbatim(0)}? verbatim[0]
       | para);
...
inline: span
      | HEX_CHAR | NORMAL_CHAR | DIGIT
      | NEWLINE | LINE_BREAK | SPACE | TAB | SPECIAL_CHAR
      | EMPH | UNDERSCORE
      | COLON | SEMI_COLON
      | SLASH | PERIOD
      | OPEN_ANGLE_BRACKET | CLOSE_ANGLE_BRACKET
      | OPEN_PAREN | CLOSE_PAREN
      | EXCLAMATION_MARK
      | SHARP
      | OPEN_SB | CLOSE_SB
      | AMPERSAND
      | BACKSLASH
      | SINGLE_QUOTE | DOUBLE_QUOTE
      | BACKTICK | PLUS | MINUS | OPEN_CURLY | CLOSE_CURLY
      | AT;
...
