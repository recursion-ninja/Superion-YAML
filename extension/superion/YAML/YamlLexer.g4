lexer grammar YamlLexer;

tokens { INDENT, DEDENT }

@lexer::members {
  private java.util.LinkedList<Token> tokens = new java.util.LinkedList<>();

  // The stack that keeps track of the indentation level.
  private java.util.Stack<Integer> indents = new java.util.Stack<>();

  // The amount of opened braces, brackets and parenthesis.
  private int opened = 0;

  // The most recently produced token.
  private Token lastToken = null;

  public void schedule(Token t) {
    tokens.offer(t);
  }

  @Override
  public Token nextToken() {

      if (tokens.isEmpty()) {
          Token next = super.nextToken();

          if (next.getType() == EOF) {
              processEOF_NextToken();
              next = tokens.poll();
          } else if(next.getType() == NEWLINE) {
              processNEWLINE_NextToken();
              next = tokens.poll();
          }

          if (lastToken != null && lastToken.getType() == MINUS) {
              switch(next.getType()) {
                  case MINUS:
                    next = commonToken(YamlParser.NEWLINE, "\n");
                    createAndScheduleIndent(this._tokenStartCharPositionInLine);
                    schedule(commonToken(YamlParser.MINUS, "-"));
                  break;
                  case STRING_MY:
                    int indent = this._tokenStartCharPositionInLine;
                    Token afterNext = super.nextToken();
                    if(afterNext.getType() == COLON) {
                        createAndScheduleIndent(indent);
                        schedule(next);
                        next = commonToken(YamlParser.NEWLINE, "\n");
                    }

                    if (afterNext.getType() == EOF) {
                        processEOF_NextToken();
                    } else if(afterNext.getType() == NEWLINE) {
                        processNEWLINE_NextToken();
                    }
                    else {
                        schedule(afterNext);
                    }
                    break;
              }
          }

          this.lastToken = next;
      } else {
          this.lastToken = tokens.poll();
      }

      return this.lastToken;
  }

  private void processEOF_NextToken() {
      schedule(commonToken(YamlParser.NEWLINE, "\n"));

      if (!this.indents.isEmpty()) {

          // Now emit as much DEDENT tokens as needed.
          while (!indents.isEmpty()) {
              this.schedule(createDedent());
              indents.pop();
          }
      }

      // Put the EOF back on the token stream.
      this.schedule(commonToken(YamlParser.EOF, "<EOF>"));
  }

  private void processNEWLINE_NextToken() {
      String spaces = getText().replaceAll("[\r\n]+", "");

      schedule(commonToken(NEWLINE, "\n"));

      int indent = getIndentationCount(spaces);
      int previous = indents.isEmpty() ? 0 : indents.peek();

      if (indent == previous) {
          // skip indents of the same size as the present indent-size
          //skip();
      }
      else if (indent > previous) {
          indents.push(indent);
          schedule(commonToken(YamlParser.INDENT, spaces));
      }
      else {
          // Possibly emit more than 1 DEDENT token.
          while(!indents.isEmpty() && indents.peek() > indent) {
              this.schedule(createDedent());
              indents.pop();
          }
      }

  }

  private Token createDedent() {
    CommonToken dedent = commonToken(YamlParser.DEDENT, "");
    dedent.setLine(this.lastToken.getLine());
    return dedent;
  }

  private CommonToken commonToken(int type, String text) {
    int stop = this.getCharIndex() - 1;
    int start = text.isEmpty() ? stop : stop - text.length() + 1;
    return new CommonToken(this._tokenFactorySourcePair, type, DEFAULT_TOKEN_CHANNEL, start, stop);
  }

  private void createAndScheduleIndent(int indent) {
      int previous = indents.isEmpty() ? 0 : indents.peek();
      if (indent > previous) {
          indents.push(indent);
          schedule(commonToken(YamlParser.INDENT, "-"));
      }
  }

  // Calculates the indentation of the provided spaces
  static int getIndentationCount(String spaces) {

    int count = 0;

    for (char ch : spaces.toCharArray()) {
      switch (ch) {
        case '\t':
          count += 8 - (count % 8);
          break;
        default:
          // A normal space char.
          count++;
      }
    }

    return count;
  }

  boolean atStartOfInput() {
    return super.getCharPositionInLine() == 0 && super.getLine() == 1;
  }

  // Indentation of a string literal. '-1' means the value is not set.
  private int string_literal_start = -1;

}


// lexer rules
NEWLINE
 : ( {atStartOfInput()}?   SPACES
   | ( '\r'? '\n' | '\r' ) SPACES?
   )
   {
     int next = _input.LA(1);
     if (opened > 0 || next == '\r' || next == '\n' || next == '#') {
         // If we're inside a list or on a blank line, ignore all indents,
         // dedents and line breaks.
         skip();
     }
   }
 ;

BYTES_LITERAL
 : [bB] [rR]? ( SHORT_BYTES | LONG_BYTES )
 ;

DECIMAL_INTEGER
 : NON_ZERO_DIGIT DIGIT*
 | '0'+
 ;

OCT_INTEGER
 : '0' [oO] OCT_DIGIT+
 ;

HEX_INTEGER
 : '0' [xX] HEX_DIGIT+
 ;

BIN_INTEGER
 : '0' [bB] BIN_DIGIT+
 ;

FLOAT_NUMBER
 : POINT_FLOAT
 | EXPONENT_FLOAT
 ;

IMAG_NUMBER
 : ( FLOAT_NUMBER | INT_PART ) [jJ]
 ;

MINUS:              '-';
DOCUMENTSTART:      { super.getCharPositionInLine() == 0 }? '---';
DOCUMENTEND:        { super.getCharPositionInLine() == 0 }? '...';
AMPERSAND :         '&';
STAR :              '*';
OPEN_PAREN :        '(' {opened++;};
CLOSE_PAREN :       ')' {opened--;};
COMMA :             ',';
COLON :             ':';
OPEN_BRACK :        '[' {opened++;} -> pushMode(FLOW);
CLOSE_BRACK :       ']' {opened--;};
OPEN_BRACE :        '{' {opened++;}  -> pushMode(FLOW);
CLOSE_BRACE :       '}' {opened--;};
LITERIAL_STR_IND:   '|' {string_literal_start=-1;} -> pushMode(LITERAL_STRING);
FOLD_STR_IND:       '>' {string_literal_start=-1;} -> pushMode(LITERAL_STRING);
DOUBLE_QUOTE:       '"' -> pushMode(DOUBLE_QUOTE_STR);

ANCHOR
 : AMPERSAND NAME
 ;

ALIAS
 : STAR NAME
 ;

fragment NAME
 : [A-Za-z0-9]+
 ;

STRING_MY
 : STRING_MY_START (~(' '|'\r'|'\n'|'"'|':') | (':' ~[ \r\n]) | (' '+ ~[ :#\r\n]) )*
 ;

fragment STRING_MY_START
 : ~('-'|' '|'\r'|'\n'|'"'|':'|'#'|'['|'{'|'&'|'*'|'|') | (':' ~[ \r\n]) | ('-' ~[ \-\r\n]) | {_input.LA(3) != 45}? '--' | {super.getCharPositionInLine() != 0}? '---'
 ;

SKIP1
 : ( SPACES | COMMENT | LINE_JOINING ) -> skip
 ;

UNKNOWN_CHAR
 : .
 ;

// fragments
fragment NON_ZERO_DIGIT
 : [1-9]
 ;

fragment DIGIT
 : [0-9]
 ;

fragment OCT_DIGIT
 : [0-7]
 ;

fragment HEX_DIGIT
 : [0-9a-fA-F]
 ;

fragment BIN_DIGIT
 : [01]
 ;

fragment POINT_FLOAT
 : INT_PART? FRACTION
 | INT_PART '.'
 ;

fragment EXPONENT_FLOAT
 : ( INT_PART | POINT_FLOAT ) EXPONENT
 ;

fragment INT_PART
 : DIGIT+
 ;

fragment FRACTION
 : '.' DIGIT+
 ;

fragment EXPONENT
 : [eE] [+-]? DIGIT+
 ;

fragment SHORT_BYTES
 : '\'' ( SHORT_BYTES_CHAR_NO_SINGLE_QUOTE | BYTES_ESCAPE_SEQ )* '\''
 | '"' ( SHORT_BYTES_CHAR_NO_DOUBLE_QUOTE | BYTES_ESCAPE_SEQ )* '"'
 ;

fragment LONG_BYTES
 : '\'\'\'' LONG_BYTES_ITEM*? '\'\'\''
 | '"""' LONG_BYTES_ITEM*? '"""'
 ;

fragment LONG_BYTES_ITEM
 : LONG_BYTES_CHAR
 | BYTES_ESCAPE_SEQ
 ;

fragment SHORT_BYTES_CHAR_NO_SINGLE_QUOTE
 : [\u0000-\u0009]
 | [\u000B-\u000C]
 | [\u000E-\u0026]
 | [\u0028-\u005B]
 | [\u005D-\u007F]
 ;

fragment SHORT_BYTES_CHAR_NO_DOUBLE_QUOTE
 : [\u0000-\u0009]
 | [\u000B-\u000C]
 | [\u000E-\u0021]
 | [\u0023-\u005B]
 | [\u005D-\u007F]
 ;

fragment LONG_BYTES_CHAR
 : [\u0000-\u005B]
 | [\u005D-\u007F]
 ;

fragment BYTES_ESCAPE_SEQ
 : '\\' [\u0000-\u007F]
 ;

fragment SPACES
 : [ \t]+
 ;

fragment COMMENT
 : '#' ~[\r\n]*
 ;

fragment LINE_JOINING
 : '\\' SPACES? ( '\r'? '\n' | '\r' )
 ;

// MODE CHANGE
mode FLOW;

DECIMAL_INTEGER2
 : (NON_ZERO_DIGIT DIGIT*
 | '0'+)                     -> type(DECIMAL_INTEGER)
 ;

OCT_INTEGER2
 : '0' [oO] OCT_DIGIT+      -> type(OCT_INTEGER)
 ;

HEX_INTEGER2
 : '0' [xX] HEX_DIGIT+      -> type(HEX_INTEGER)
 ;

BIN_INTEGER2
 : '0' [bB] BIN_DIGIT+      -> type(BIN_INTEGER)
 ;

FLOAT_NUMBER2
 : (POINT_FLOAT
 | EXPONENT_FLOAT)           -> type(FLOAT_NUMBER)
 ;

IMAG_NUMBER2
 : ( FLOAT_NUMBER | INT_PART ) [jJ]     -> type(IMAG_NUMBER)
 ;

STRING_MY_2
 : STRING_MY_START_2 (~(' '|'\r'|'\n'|'"'|':'|'['|']'|','|'{'|'}') | (':' ~[ \r\n]) | (' '+ ~(' '|':'|'#'|'\r'|'\n'|'['|']'|','|'{'|'}')) )* -> type(STRING_MY)
 ;

fragment STRING_MY_START_2
 : ~('-'|' '|'\r'|'\n'|'"'|':'|'#'|'['|']'|','|'{'|'}') | (':' ~[ \r\n]) | ('-' ~[ \r\n])
 ;

COMMA2
 : ',' -> type(COMMA)
 ;

COLON2
 : ':' -> type(COLON)
 ;

SKIP2
 : ( SPACES | COMMENT | LINE_JOINING | ( '\r'? '\n' | '\r' )) -> skip
 ;

CLOSE_BRACK2
 : ']'  -> type(CLOSE_BRACK), popMode
 ;

CLOSE_BRACE2
 : '}'  -> type(CLOSE_BRACE), popMode
 ;

// MODE CHANGE
mode LITERAL_STRING;

NEWLINE_STR_LITERAL: ( '\r'? '\n' | '\r' ) [ \t]*
   {
     int next = _input.LA(1);
     if(!(opened > 0 || next == '\r' || next == '\n' || next == '#')) {
       int indent = indents.isEmpty() ? 0 : indents.peek();
       int space_count = getIndentationCount(getText().replaceAll("[\r\n]+", ""));
       if(space_count <= indent) {
         popMode();
         setType(NEWLINE);
       }
       else if (string_literal_start == -1) {
         string_literal_start = space_count;
       }
     }
   }
 ;

STRING_MY_3
 : ~('\r'|'\n')*
   {
     int extraSpace = _tokenStartCharPositionInLine - string_literal_start;
     if(extraSpace > 0) {
       StringBuilder builder = new StringBuilder();
       for (int i = 0; i < extraSpace; i++) {
         builder.append(' ');
       }
       builder.append(getText());
       setText(builder.toString());
     }
   }
   -> type(STRING_MY)
 ;


// MODE CHANGE
mode DOUBLE_QUOTE_STR;

STRING_MY_4
 : (~["\r\n\\] | '\\\\')*
   -> type(STRING_MY)
 ;

NEWLINE_STR_QUOTE: ( '\r'? '\n' | '\r' ) [ \t]* ;

DOUBLE_QUOTE2: '"' -> type(DOUBLE_QUOTE), popMode;
