parser grammar MarkdownParser;
options {
    tokenVocab=MarkdownLexer;
}

@members {
    private static final boolean debug = false;

    private boolean tokenStartsWith(int la, String value) {
        Token t = _input.LT(la);
        return t.getText().startsWith(value);
    }

    private boolean tokenEndsWith(int la, String value) {
        Token t = _input.LT(la);
        return t.getText().endsWith(value);
    }

    private boolean tokenIs(int la, int type) {
        Token t = _input.LT(la);
        return t.getType() == type;
    }

    private int[] countSpaces(int index, int level, boolean skipBL) {
        int la = index;
        int[] retValue = new int[]{-1, -1};
        while (skipBL && _input.LT(la).getType() == BLANK_LINE) {
            la++;
        }
        if (_input.LT(la).getCharPositionInLine() != 0)
            return retValue;
        int spaces = 0;
        int type = 0;
        do {
            Token t = _input.LT(la++);
            type = t.getType();
            if (type == SPACE)
                spaces++;
            else if (type == TAB)
                spaces += 4;
    	} while (type == SPACE || type == TAB);
    	retValue[0] = spaces;
    	retValue[1] = type;
    	return retValue;
    }

    private boolean followListItem(int index, int level) {
        return followListItem(index, level, false);
    }

    private boolean followListItem(int index, int level, boolean skipBL) {
        if (level < 0)
            return false;

        int[] spacesAndType = countSpaces(index, level, skipBL);
        int spaces = spacesAndType[0];
        int type = spacesAndType[1];

    	int minSpaces = level*4;
    	int maxSpaces = minSpaces+3;
    	
    	boolean retValue = spaces >= minSpaces && spaces <= maxSpaces;
    	if (retValue) {   // Check also token type.
    	    // TODO: checking for DIGIT is an incomplete check.
    	    retValue = (type == EMPH || type == MINUS || type == PLUS || type == DIGIT);
    	}
    	if (debug)
    	    System.out.println("followListItem(" + level
    	    + "): line: " + _input.LT(index).getLine()
    	    + ", skipBL: " + skipBL
    	    + ", " + retValue);
    	return retValue;
    }

    private boolean followVerbatim(int level) {
        return followVerbatim(level, false);
    }

    private boolean followVerbatim(int level, boolean skipBL) {
        if (level < 0)
            return false;

        int[] spacesAndType = countSpaces(1, level, skipBL);
        int spaces = spacesAndType[0];

    	boolean retValue = spaces >= (level+1)*4;

        if (debug)
    	    System.out.println("followVerbatim(" + level
    	    + "): line: " + _input.LT(1).getLine()
    	    + ", skipBL: " + skipBL
    	    + ", " + retValue);
    	
    	return retValue;
    }

    // List item continuation should have same number of spaces of verbatim with the lower level.
    private boolean followContinuation(int level) {
        return followContinuation(level, false);
    }

    private boolean followContinuation(int level, boolean skipBL) {
        if (level < 0)
            return false;

        int[] spacesAndType = countSpaces(1, level, skipBL);
        int spaces = spacesAndType[0];

    	boolean retValue = spaces >= (level+1)*4;

        if (debug)
    	    System.out.println("followContinuation(" + level
    	    + "): line: " + _input.LT(1).getLine()
    	    + ", skipBL: " + skipBL
    	    + ", " + retValue);
    	
    	return retValue;
    }

    private boolean followBlockquote(int level) {
        if (level < 0)
            return false;

        int[] spacesAndType = countSpaces(1, level, true);
        int spaces = spacesAndType[0];
        int type = spacesAndType[1];

    	int minSpaces = level*4;
    	int maxSpaces = minSpaces+3;
    	boolean retValue = spaces >= minSpaces && spaces <= maxSpaces && type == CLOSE_ANGLE_BRACKET;

        if (debug)
            System.out.println("followBlockquote(" + level
            + "): line: " + _input.LT(1).getLine()
            + ", " + retValue);
    	
    	return retValue;
    }
}


document: block+ BLANK_LINE* EOF?;

block: BLANK_LINE*   // Blank lines at the begin of the document or extra blank lines between blocks are matched here.
       (htmlBlockTags   // Match HTML at level block to do not add extra <p> if paragraph contains only HTML.
       | htmlBlockSelfClosing
       | htmlComment
       | heading
       | horizontalRule
       | blockQuote[0]
       | references
       | {followListItem(1, 0)}? orderedList[0]
       | {followListItem(1, 0)}? bulletList[0]
       | {followVerbatim(0)}? verbatim[0]   // After lists because deep lists can start with lot of spaces.
       | para
       )
     ;

// Block level HTML
// Same as inline HTML but with trailing space and new line.
htmlBlockTags: htmlBlockInTags (SPACE | TAB)* (NEWLINE | LINE_BREAK);
htmlBlockSelfClosing: htmlBlockInSelfClosing  (SPACE | TAB)* (NEWLINE | LINE_BREAK);

// Heading
heading: setextHeading | atxHeading;
setextHeading: setextHeading1 | setextHeading2;
// Should verify ~NEWLINE after each inline
setextHeading1: ({!tokenIs(1, NEWLINE)}? inline)+ NEWLINE SETEXT_BOTTOM_1;
setextHeading2: ({!tokenIs(1, NEWLINE)}? inline)+ NEWLINE SETEXT_BOTTOM_2;
atxHeading: ATX_START (SPACE | TAB) inline+? ((SPACE | TAB)*)? (SHARP*)? (SPACE | TAB)? (NEWLINE | LINE_BREAK);
rawLine: (~NEWLINE)* NEWLINE;
nonIndentSpace: SPACE? SPACE? SPACE?;
blockQuote[int _level]: {followBlockquote($_level)}?    //{_input.LT(1).getCharPositionInLine() == 0}?
                        (blockQuoteBlankLine? (SPACE | TAB)* CLOSE_ANGLE_BRACKET (SPACE | TAB)? rawLine)+;
blockQuoteBlankLine: BLANK_LINE+;
verbatim[int _level]: ({followVerbatim($_level, true)}? verbatimBlankLine* rawLine)+;
verbatimBlankLine: BLANK_LINE;
horizontalRule: nonIndentSpace
                (EMPH (SPACE | TAB)? EMPH (SPACE | TAB)? EMPH ((SPACE | TAB)? EMPH)*
              | MINUS (SPACE | TAB)? MINUS (SPACE | TAB)? MINUS ((SPACE | TAB)? MINUS)*
              | UNDERSCORE (SPACE | TAB)? UNDERSCORE (SPACE | TAB)? UNDERSCORE ((SPACE | TAB)? UNDERSCORE)*
              ) NEWLINE | SETEXT_BOTTOM_2;

// References
references: (reference)+;
reference: nonIndentSpace referenceLabel
           COLON (SPACE | TAB)+ referenceUrl
           (((SPACE | TAB)+ | (SPACE | TAB)* NEWLINE (SPACE | TAB)*)   // Same or next line.
           referenceTitle)?
           (SPACE | TAB)* NEWLINE;
referenceLabel: OPEN_SB referenceId CLOSE_SB;
referenceId: (~(NEWLINE | CLOSE_SB))+;
referenceUrl: OPEN_ANGLE_BRACKET .*? CLOSE_ANGLE_BRACKET
            | (~(SPACE | TAB | NEWLINE | OPEN_ANGLE_BRACKET | CLOSE_ANGLE_BRACKET))+
            ;
referenceTitle: referenceTitleSingle | referenceTitleDouble | referenceTitleParens; 
referenceTitleSingle: SINGLE_QUOTE (~(NEWLINE | SINGLE_QUOTE))*
                      SINGLE_QUOTE;
referenceTitleDouble: DOUBLE_QUOTE (~(NEWLINE | DOUBLE_QUOTE))*
                      DOUBLE_QUOTE;
referenceTitleParens: OPEN_PAREN (~(NEWLINE | CLOSE_PAREN))*
                      CLOSE_PAREN;

// Lists
// The main rules for lists.
// nextItemWithPar is used by MarkdownTranslator to track which items have BLANK_LINE* .
orderedList[int _level]
locals[boolean nextItemWithPar]
       : ({followListItem(1, $_level, true)}? orderedListItem[_level]
         )+;

bulletList[int _level]
locals[boolean nextItemWithPar]
       : ({followListItem(1, $_level, true)}? bulletListItem[_level]
         )+;

orderedListItem[int _level]
locals[boolean isWithPar]
       : BLANK_LINE* (SPACE | TAB)* DIGIT+ PERIOD (SPACE | TAB)+
         inlineListItem[_level]
         // After the begin we can have a sublist without blank lines or a blank line with list, para or verbatim.
         (
             {followListItem(1, $_level+1)}? (orderedList[_level+1] | bulletList[_level+1])
             | ({followVerbatim($_level+1, true) || followListItem(1, $_level+1, true) || followContinuation($_level, true)}?
                listItemBlankLine
                   (
                       {followVerbatim($_level+1, true)}? verbatim[_level+1]
                       | {followListItem(1, $_level+1, true)}? (orderedList[_level+1] | bulletList[_level+1])
                       | {followBlockquote($_level+1)}? blockQuote[_level+1]
                       | {followContinuation($_level, true)}? (SPACE | TAB)* inlineListItem[_level]
                   )
               )+
         )?
         ;   // BLANK_LINE*
//
bulletListItem[int _level]
locals[boolean isWithPar]
       : BLANK_LINE* (SPACE | TAB)* (PLUS | MINUS | EMPH) (SPACE | TAB)+
         inlineListItem[_level]
         // After the begin we can have a sublist without blank lines or a blank line with list, para or verbatim.
         (
             {followListItem(1, $_level+1)}? (orderedList[_level+1] | bulletList[_level+1])
             | ({followVerbatim($_level+1, true) || followListItem(1, $_level+1, true) || followContinuation($_level, true)}?
                listItemBlankLine
                   (
                       {followVerbatim($_level+1, true)}? verbatim[_level+1]
                       | {followListItem(1, $_level+1, true)}? (orderedList[_level+1] | bulletList[_level+1])
                       | {followBlockquote($_level+1)}? blockQuote[_level+1]
                       | {followContinuation($_level, true)}? (SPACE | TAB)* inlineListItem[_level]
                   )
               )+
         )?
         ;   // BLANK_LINE*

inlineListItem[int _level]
       : ({!followListItem(1, $_level) && !followListItem(1, $_level-1) && !followListItem(1, $_level+1)}?
              inline
         )+;

listItemBlankLine: BLANK_LINE+;


para: nonIndentSpace inline+ NEWLINE;

// In line content.
// In line can be span or one of the special char used for span that should be left has alternative
// in case matching a span fails.
inline: span   // Max priority, single char matching should be choosed only when span fails.
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
      | D | I | V | S | P | A | N
      | H | R
      | AT
      ;

//Span elements.
span: strong
    | emph
    | image
    | link
    | code
    | htmlBlockInTags
    | htmlBlockInSelfClosing
    | autolink
    | entity
    | escapedChar
    ;


emph: emphStar | emphUl;
emphStar: {!tokenStartsWith(2, " ") && !tokenIs(2, EMPH)}?
          EMPH inline+? EMPH;
emphUl: {!tokenStartsWith(2, " ") && !tokenIs(2, UNDERSCORE)}?
        UNDERSCORE (inline)+ UNDERSCORE;

strong: strongStar | strongUl;
strongStar: {!tokenStartsWith(3, " ")}?
            EMPH EMPH inline+? EMPH EMPH;
strongUl: {!tokenStartsWith(3, " ")}?
          UNDERSCORE UNDERSCORE (inline)+ UNDERSCORE UNDERSCORE;

// Image and link
image: EXCLAMATION_MARK imageLink;
imageLink: imageAlt (explicitImageLink | referenceImageLink);
explicitImageLink: OPEN_PAREN linkUrl ((SPACE | TAB)+ linkTitle)? CLOSE_PAREN;
imageAlt: OPEN_SB (~(NEWLINE | CLOSE_SB))* CLOSE_SB;
referenceImageLink: OPEN_SB referenceId CLOSE_SB;

link: linkContent (explicitLink | referenceLink)?;
linkContent: OPEN_SB inline+? CLOSE_SB;
explicitLink: OPEN_PAREN linkUrl? ((SPACE | TAB)+ linkTitle)? (SPACE | TAB)* CLOSE_PAREN;
linkUrl: (~(SPACE | TAB | NEWLINE | CLOSE_PAREN))+;
linkTitle: linkTitleSingle | linkTitleDouble;
linkTitleSingle: SINGLE_QUOTE (~(NEWLINE | SINGLE_QUOTE))*
                      SINGLE_QUOTE;
linkTitleDouble: DOUBLE_QUOTE (~(NEWLINE | DOUBLE_QUOTE))*
                      DOUBLE_QUOTE;
referenceLink: (SPACE | TAB | NEWLINE)? OPEN_SB referenceId? CLOSE_SB;

// Entity
entity: AMPERSAND (SHARP hexEntityName | SHARP decEntityName | charEntityName) SEMI_COLON;
hexEntityName: (HEX_CHAR | D | A | DIGIT)+;
decEntityName: DIGIT+;
charEntityName: (HEX_CHAR | NORMAL_CHAR | D | I | V | S | P | A | N | H | R | DIGIT)+;

// Escaped char
escapedChar: BACKSLASH
           (MINUS | BACKSLASH | PERIOD | BACKTICK | EMPH | UNDERSCORE
           | EXCLAMATION_MARK | PLUS | OPEN_PAREN | CLOSE_PAREN
           | SHARP | OPEN_PAREN | CLOSE_PAREN | OPEN_CURLY | CLOSE_CURLY
           | OPEN_SB | CLOSE_SB
           | OPEN_ANGLE_BRACKET | CLOSE_ANGLE_BRACKET
           );

// Code
// Order matter
code: spaceBacktickCode | doubleBacktickCode | backtickCode;

backtickCode: {!tokenIs(2, BACKTICK)}? BACKTICK ({!tokenIs(1, BLANK_LINE)}? .)+? BACKTICK;

doubleBacktickCode: {!tokenIs(3, SPACE) && !tokenIs(3, TAB)}? BACKTICK BACKTICK ({!tokenIs(1, BLANK_LINE)}? .)+? BACKTICK BACKTICK;

spaceBacktickCode: BACKTICK BACKTICK SPACE ({!tokenIs(1, BLANK_LINE)}? .)+? SPACE BACKTICK BACKTICK;

// HTML
attributeName: (NORMAL_CHAR | HEX_CHAR | D | I | V | S | P | A | N | H | R)+;
htmlAttributeS: attributeName EQUAL SINGLE_QUOTE .*? SINGLE_QUOTE;
htmlAttributeD: attributeName EQUAL DOUBLE_QUOTE .*? DOUBLE_QUOTE;

htmlBlockOpenDiv: OPEN_ANGLE_BRACKET D I V ((SPACE | TAB)+ (htmlAttributeS | htmlAttributeD))* (SPACE | TAB)* CLOSE_ANGLE_BRACKET;
htmlBlockCloseDiv: OPEN_ANGLE_BRACKET SLASH D I V (SPACE | TAB)* CLOSE_ANGLE_BRACKET;
htmlBlockDiv: htmlBlockOpenDiv (htmlBlockDiv | .)*? htmlBlockCloseDiv;

htmlBlockOpenSpan: OPEN_ANGLE_BRACKET S P A N ((SPACE | TAB)+ (htmlAttributeS | htmlAttributeD))* (SPACE | TAB)* CLOSE_ANGLE_BRACKET;
htmlBlockCloseSpan: OPEN_ANGLE_BRACKET SLASH S P A N (SPACE | TAB)* CLOSE_ANGLE_BRACKET;
htmlBlockSpan: htmlBlockOpenSpan (htmlBlockSpan | .)*? htmlBlockCloseSpan;

htmlBlockHr: OPEN_ANGLE_BRACKET H R ((SPACE | TAB)+ (htmlAttributeS | htmlAttributeD))* (SPACE | TAB)* SLASH? (SPACE | TAB)*
             CLOSE_ANGLE_BRACKET;

htmlBlockInTags: htmlBlockDiv
               | htmlBlockSpan
               | htmlBlockHr
               ;

htmlBlockInSelfClosing: OPEN_ANGLE_BRACKET attributeName
                      ((SPACE | TAB)+ (htmlAttributeS | htmlAttributeD))* (SPACE | TAB)* SLASH CLOSE_ANGLE_BRACKET;

// HTML comment
htmlComment: HTML_COMMENT_OPEN .*? MINUS MINUS CLOSE_ANGLE_BRACKET (SPACE | TAB)* (NEWLINE | LINE_BREAK);

// Autolinks
autolink: OPEN_ANGLE_BRACKET (~(BLANK_LINE | CLOSE_ANGLE_BRACKET))+ CLOSE_ANGLE_BRACKET;
