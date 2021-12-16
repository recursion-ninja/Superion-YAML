parser grammar MarkdownParser;
options { tokenVocab=MarkdownLexer; }
@members {
    private static final boolean debug = false;
    private boolean tokenStartsWith(int la, String value) {
        Token t = _input.LT(la);
        return t.getText().startsWith(value);
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
       | para
       )
     ;
// Block level HTML
htmlBlockTags: htmlBlockInTags (SPACE | TAB)* (NEWLINE | LINE_BREAK);
htmlBlockSelfClosing: htmlBlockInSelfClosing  (SPACE | TAB)* (NEWLINE | LINE_BREAK);
...
// Heading
heading: setextHeading | atxHeading;
setextHeading: setextHeading1 | setextHeading2;
setextHeading1: ({!tokenIs(1, NEWLINE)}? inline)+ NEWLINE SETEXT_BOTTOM_1;
setextHeading2: ({!tokenIs(1, NEWLINE)}? inline)+ NEWLINE SETEXT_BOTTOM_2;
...