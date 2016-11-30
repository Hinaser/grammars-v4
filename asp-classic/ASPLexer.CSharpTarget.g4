/*
ASP grammar.
The MIT License (MIT).
Copyright (c) 2015-2016, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

lexer grammar ASPLexer;

channels { AspComments, ErrorLexem }

@lexer::members
{
bool _scriptTag;
bool _styleTag;
int _prevTokenType;
int _prevMeanTokenType;
string _htmlNameText;
string _htmlAttrName;
bool _aspScript;
bool _insideString;
bool _HtmlNameConsumed = false;
bool _HtmlAttrNameConsumed = false;
bool _HtmlEventNameConsumed = false;

public override IToken NextToken()
{
    CommonToken token = (CommonToken)base.NextToken();

    if (token.Type == ASPEnd || token.Type == ASPEndSingleLineComment)
    {
        if (_mode == SingleLineCommentMode)
        {
            // SingleLineCommentMode for such allowed syntax:
            // <% echo "Hello world"; ' comment %>
            token.Type = ASPEnd;
            PopMode(); // exit from SingleLineComment mode.
        }
        PopMode(); // exit from ASP mode.
        
        if (token.Text.ToLower() == "</script>")
        {
            _aspScript = false;
        }
    }
    else if (token.Type == HtmlName || token.Type == HtmlScriptOpen || token.Type == HtmlStyleOpen)
    {
        if(token.Type == HtmlName){
            _htmlNameText = token.Text;
        }
        
        if(!_HtmlNameConsumed){
            _HtmlNameConsumed = true;
            _HtmlAttrNameConsumed = false;
        }
    }
    else if (token.Type == HtmlAttrName){
        if(_HtmlAttrNameConsumed){
            token.Type = HtmlAttrVal;
            _HtmlAttrNameConsumed = false;
        }
        else{
            if(token.Text.Substring(0, 2).ToLower() == "on"){
                token.Type = HtmlEventName;
                _HtmlEventNameConsumed = true;
            }
            else{
                _htmlAttrName = token.Text;
                _HtmlAttrNameConsumed = true;
            }
        }
    }
    else if (token.Type == HtmlAttrVal)
    {
        if(_HtmlAttrNameConsumed){
            _HtmlAttrNameConsumed = false;
        }
        else if(_HtmlEventNameConsumed){
            _HtmlEventNameConsumed = false;
            token.Type = HtmlEventVal;
        }
    }
    else if (token.Type == HtmlEventName){
        _HtmlEventNameConsumed = true;
    }
    else if (token.Type == HtmlEventVal){
        _HtmlEventNameConsumed = false;
    }
    else if (token.Type == ServerScript){
        token.Type = ASPStart;
        _aspScript = true;
        PushMode(ASP);
    }
    else if (_mode == ASP)
    {
        switch(token.Type)
        {
            // Token before Statement must be VB_NEWLINE or ASPStart
            case VB_CHDIR:
            case VB_CHDRIVE:
            case VB_CLOSE:
            case VB_END:
            case VB_ERASE:
            case VB_FILECOPY:
            case VB_GLOBAL:
            case VB_INPUT:
            case VB_LOAD:
            case VB_MKDIR:
            case VB_NAME:
            case VB_OPEN:
            case VB_PRINT:
            case VB_PUT:
            case VB_REDIM:
            case VB_RANDOMIZE:
            case VB_RESET:
            case VB_RETURN:
            case VB_RMDIR:
            case VB_RSET:
            case VB_SAVEPICTURE:
            case VB_SAVESETTING:
            case VB_SEEK:
            case VB_SELECT:
            case VB_SENDKEYS:
            case VB_SETATTR:
            case VB_SET:
            case VB_STOP:
            case VB_TYPEOF:
            case VB_UNLOAD:
            case VB_UNLOCK:
            case VB_WRITE:
                if(_prevTokenType != VB_NEWLINE && _prevTokenType != ASPStart)
                {
                    token.Type = VB_IDENTIFIER;
                }
                break;
            case VB_MID:
                int maxIndex = _input.Size - 1;
                System.Text.RegularExpressions.Regex r = new System.Text.RegularExpressions.Regex(@"[ \t]");
                for(int i=0;i<maxIndex;i++)
                {
                    if(r.IsMatch(new string(new char[]{(char)_input.La(i)}))) continue;
                    else if((char)_input.La(i) == '(') break;
                    
                    token.Type = VB_IDENTIFIER;
                    break;
                }
                break;
            case VB_DATE:
                if(_prevMeanTokenType != VB_AS && _prevMeanTokenType != VB_IS)
                {
                    token.Type = VB_IDENTIFIER;
                }
                else if(_prevTokenType != VB_NEWLINE && _prevTokenType != ASPStart)
                {
                    token.Type = VB_IDENTIFIER;
                }
                break;
                break;
            case VB_IDENTIFIER:
                if(token.Text.ToUpper() == "REM")
                {
                    token.Type = VB_COMMENT_START;
                    PushMode(SingleLineCommentMode);
                    token.Channel = AspComments;
                }
                break;
            case VB_COLON:
                if (_input.Index + 3 > _input.Size - 1) break;
                char[] rem = new char[3];
                rem[0] = (char)_input.La(1);
                rem[1] = (char)_input.La(2);
                rem[2] = (char)_input.La(3);
                if ((new string(rem)).ToUpper() == "REM")
                {
                    token = (CommonToken)NextToken();
                }
                break;
            case VB_TEXT:
                if(_prevMeanTokenType != VB_OPTION_COMPARE)
                {
                    token.Type = VB_IDENTIFIER;
                }
                break;
        }

        if (_channel != Hidden)
        {
            _prevTokenType = token.Type;

            if(token.Type != VB_NEWLINE && token.Type != VB_WS){
                _prevMeanTokenType = token.Type;
            }
        }
    }

    return token;
}
}

SeaWhitespace:  [ \t\r\n]+ /*-> channel(HIDDEN)*/;
HtmlText:       ~[<]+;
ASPStartEcho:   AspStartEchoFragment -> /*type(Echo), */pushMode(ASP);
ASPStart:       AspStartFragment -> /*skip, */pushMode(ASP);
ASPStartDirective: '<%' SeaWhitespace? '@' SeaWhitespace? -> pushMode(ASPDirective);
HtmlScriptOpen: '<' S C R I P T { _scriptTag = true; _HtmlNameConsumed = false; } -> pushMode(INSIDE);
HtmlStyleOpen:  '<' S T Y L E { _styleTag = true; _HtmlNameConsumed = false; } -> pushMode(INSIDE);
HtmlComment:    '<' '!' '--' .*? '-->';
HtmlDtd:        '<' '!' .*? '>';
HtmlOpen:       '<' { _HtmlNameConsumed = false; } -> pushMode(INSIDE);
HtmlEndTag:     '</' [a-zA-Z0-9]+ [ \t\r\n\f]* '>'; // I first thought first char of tagname must not be a number, but it is not mentioned in W3C doc https://www.w3.org/TR/html5/syntax.html#syntax-tag-name

ServerScript:   '<' S C R I P T (SeaWhitespace (NameStartChar NameChar*) SeaWhitespace? HtmlEquals SeaWhitespace? (NameChar+ | '"' NameChar* '"' | '\'' NameChar* '\''))* SeaWhitespace (LangEqVbs SeaWhitespace RunatServer | RunatServer SeaWhitespace LangEqVbs) (SeaWhitespace (NameStartChar NameChar*) SeaWhitespace? HtmlEquals SeaWhitespace? (NameChar+ | '"' NameChar* '"' | '\'' NameChar* '\''))* SeaWhitespace? HtmlClose;
fragment LangEqVbs:      L A N G U A G E SeaWhitespace? VB_EQ SeaWhitespace? ('"' V B S C R I P T '"' | '\'' V B S C R I P T '\''  | V B S C R I P T);
fragment RunatServer:    R U N A T SeaWhitespace? VB_EQ SeaWhitespace? ('"' S E R V E R '"' | '\'' S E R V E R '\''  | S E R V E R);

Error:          .         -> channel(ErrorLexem);

mode INSIDE;

ASPStartEchoInside: AspStartEchoFragment -> /*type(Echo), */pushMode(ASP), type(ASPStartEcho);
ASPStartInside:     AspStartFragment -> /*skip, */pushMode(ASP), type(ASPStart);
HtmlClose: '>' {
_HtmlNameConsumed = false;
PopMode();
if (_scriptTag)
{
    if (!_aspScript)
    {
        PushMode(SCRIPT);
    }
    else
    {
        PushMode(ASP);
    }
    _scriptTag = false;
}
else if (_styleTag)
{
    PushMode(STYLE);
    _styleTag = false;
}
};
HtmlSlashClose: '/>' -> popMode;
HtmlSlash:      '/';
HtmlEquals:     '=';

HtmlStartQuoteString:       '\\'? '\'' -> pushMode(HtmlQuoteStringMode);
HtmlStartDoubleQuoteString: '\\'? '"'  -> pushMode(HtmlDoubleQuoteStringMode);
HtmlHex:                    '#' HexDigit+ {_HtmlNameConsumed}? -> type(HtmlAttrVal);
HtmlDecimal:                Digit+ {_HtmlNameConsumed}? -> type(HtmlAttrVal);
HtmlSpace:                  [ \t\r\n]+ /*-> channel(HIDDEN)*/;
HtmlName:                   {!_HtmlNameConsumed}? NameStartChar NameChar*;
HtmlAttrName:               {_HtmlNameConsumed}? NameStartChar NameChar*;
HtmlEventName:              {_HtmlNameConsumed}? 'on' NameStartChar NameChar*;
HtmlAttrVal:                {_HtmlAttrNameConsumed}? NameChar+;
HtmlEventVal:               {_HtmlEventNameConsumed}? InlineJsChar+;
ErrorInside:                .          -> channel(ErrorLexem);

mode HtmlQuoteStringMode;

ASPStartEchoInsideQuoteString: AspStartEchoFragment -> /*type(Echo), */pushMode(ASP), type(ASPStartEcho);
ASPStartInsideQuoteString:     AspStartFragment -> /*skip, */pushMode(ASP), type(ASPStart);
HtmlEndQuoteString:            '\'' '\''? -> popMode;
HtmlQuoteString:               ~[<']+ -> type(HtmlAttrVal); /* Attr name must not contain quote string. https://www.w3.org/TR/html-markup/syntax.html#attribute */
ErrorHtmlQuote:                .          -> channel(ErrorLexem);

mode HtmlDoubleQuoteStringMode;

ASPStartEchoDoubleQuoteString: AspStartEchoFragment -> /*type(Echo), */pushMode(ASP), type(ASPStartEcho);
ASPStartDoubleQuoteString:     AspStartFragment -> /*skip, */pushMode(ASP), type(ASPStart);
HtmlEndDoubleQuoteString:      '"' '"'? -> popMode;
HtmlDoubleQuoteString:         ~[<"]+ -> type(HtmlAttrVal); /* Attr name must not contain quote string. https://www.w3.org/TR/html-markup/syntax.html#attribute */
ErrorHtmlDoubleQuote:          .          -> channel(ErrorLexem);

// Parse JavaScript with https://github.com/antlr/grammars-v4/tree/master/ecmascript if necessary.
// Asp blocks can exist inside Script blocks too.
mode SCRIPT;

ScriptText:               ~[<]+;
ScriptClose:              '<' '/' (S C R I P T)? '>' -> popMode;
ASPStartInsideScriptEcho: AspStartEchoFragment -> /*type(Echo), */pushMode(ASP), type(ASPStartEcho);
ASPStartInsideScript:     AspStartFragment-> /*skip, */pushMode(ASP), type(ASPStart);
ScriptText2:              '<' ~[<%/]* -> type(ScriptText);
ScriptText3:              '?' ~[<]* -> type(ScriptText);
ScriptText4:              '/' ~[<]* -> type(ScriptText);

mode STYLE;

StyleText:               ~[<]+;
StyleClose:              '<' '/' (S T Y L E)? '>' -> popMode;
ASPStartInsideStyleEcho: AspStartEchoFragment -> /*type(Echo), */pushMode(ASP), type(ASPStartEcho);
ASPStartInsideStyle:     AspStartFragment-> /*skip, */pushMode(ASP), type(ASPStart);
StyleText2:              '<' ~[<%/]* -> type(StyleText);
StyleText3:              '?' ~[<]* -> type(StyleText);
StyleText4:              '/' ~[<]* -> type(StyleText);

mode ASP;

ASPEnd:              '%' '>' | {_aspScript}? '</' S C R I P T '>';
//Whitespace:         [ \t\r\n]+ -> skip;
// SingleLineComment:  '\'' -> /*skip, */more, pushMode(SingleLineCommentMode);

VB_ACCESS               : A C C E S S;
VB_ADDRESSOF            : A D D R E S S O F;
VB_ALIAS                : A L I A S;
VB_AND                  : A N D;
VB_ATTRIBUTE            : A T T R I B U T E;
VB_APPACTIVATE          : A P P A C T I V A T E;
VB_BEGIN                : B E G I N;
VB_BEEP                 : B E E P;
VB_BOOLEAN              : B O O L E A N;
VB_BYVAL                : B Y V A L;
VB_BYREF                : B Y R E F;
VB_BYTE                 : B Y T E;
VB_CALL                 : C A L L;
VB_CASE                 : C A S E;
VB_CHDIR                : C H D I R;
VB_CHDRIVE              : C H D R I V E;
VB_CLASS                : C L A S S;
VB_CLOSE                : C L O S E;
VB_COLLECTION           : C O L L E C T I O N;
VB_CONST                : C O N S T;
VB_DATE                 : D A T E;
VB_DECLARE              : D E C L A R E;
VB_DEFBOOL              : D E F B O O L;
VB_DEFBYTE              : D E F B Y T E;
VB_DEFDATE              : D E F D A T E;
VB_DEFDBL               : D E F D B L;
VB_DEFDEC               : D E F D E C;
VB_DEFCUR               : D E F C U R;
VB_DEFINT               : D E F I N T;
VB_DEFLNG               : D E F L N G;
VB_DEFOBJ               : D E F O B J;
VB_DEFSNG               : D E F S N G;
VB_DEFSTR               : D E F S T R;
VB_DEFVAR               : D E F V A R;
VB_DELETESETTING        : D E L E T E S E T T I N G;
VB_DIM                  : D I M;
VB_DO                   : D O;
VB_DOUBLE               : D O U B L E;
VB_EACH                 : E A C H;
VB_ELSE                 : E L S E;
VB_ELSEIF               : E L S E I F;
VB_END_ENUM             : E N D VB_WS E N U M;
VB_END_FUNCTION         : E N D VB_WS F U N C T I O N;
VB_END_IF               : E N D VB_WS I F;
VB_END_PROPERTY         : E N D VB_WS P R O P E R T Y;
VB_END_SELECT           : E N D VB_WS S E L E C T;
VB_END_SUB              : E N D VB_WS S U B;
VB_END_TYPE             : E N D VB_WS T Y P E;
VB_END_WITH             : E N D VB_WS W I T H;
VB_END                  : E N D;
VB_ENUM                 : E N U M;
VB_EQV                  : E Q V;
VB_ERASE                : E R A S E;
VB_ERROR                : E R R O R;
VB_EVENT                : E V E N T;
VB_EXIT_DO              : E X I T VB_WS D O;
VB_EXIT_FOR             : E X I T VB_WS F O R;
VB_EXIT_FUNCTION        : E X I T VB_WS F U N C T I O N;
VB_EXIT_PROPERTY        : E X I T VB_WS P R O P E R T Y;
VB_EXIT_SUB             : E X I T VB_WS S U B;
VB_FALSE                : F A L S E;
VB_FILECOPY             : F I L E C O P Y;
VB_FRIEND               : F R I E N D;
VB_FOR                  : F O R;
VB_FUNCTION             : F U N C T I O N;
VB_GET                  : G E T;
VB_GLOBAL               : G L O B A L;
VB_GOSUB                : G O S U B;
VB_GOTO                 : G O T O;
VB_IF                   : I F;
VB_IMP                  : I M P;
VB_IMPLEMENTS           : I M P L E M E N T S;
VB_IN                   : I N;
VB_IS                   : I S;
VB_INTEGER              : I N T E G E R;
VB_KILL                 : K I L L;
VB_LOCK                 : L O C K;
VB_LONG                 : L O N G;
VB_LOOP                 : L O O P;
VB_LET                  : L E T;
VB_LIB                  : L I B;
VB_LIKE                 : L I K E;
VB_LINE_INPUT           : L I N E ' ' I N P U T;
VB_LOAD                 : L O A D;
VB_LSET                 : L S E T;
VB_MACRO_IF             : VB_HASH I F;
VB_MACRO_ELSEIF         : VB_HASH E L S E I F;
VB_MACRO_ELSE           : VB_HASH E L S E;
VB_MACRO_END_IF         : VB_HASH E N D ' ' I F;
VB_ME                   : M E;
VB_MID                  : M I D;
VB_MKDIR                : M K D I R;
VB_MOD                  : M O D;
VB_NAME                 : N A M E;
VB_NEXT                 : N E X T;
VB_NEW                  : N E W;
VB_NOT                  : N O T;
VB_NOTHING              : N O T H I N G;
VB_NULL                 : N U L L;
VB_ON                   : O N;
VB_ON_ERROR             : O N ' ' E R R O R;
VB_OPEN                 : O P E N;
VB_OPTIONAL             : O P T I O N A L;
VB_OPTION_BASE          : O P T I O N VB_WS B A S E;
VB_OPTION_EXPLICIT      : O P T I O N VB_WS E X P L I C I T;
VB_OPTION_COMPARE       : O P T I O N VB_WS C O M P A R E;
VB_OPTION_PRIVATE_MODULE: O P T I O N VB_WS P R I V A T E ' ' M O D U L E;
VB_OR                   : O R;
VB_PARAMARRAY           : P A R A M A R R A Y;
VB_PRESERVE             : P R E S E R V E;
VB_PRINT                : P R I N T;
VB_PRIVATE              : P R I V A T E;
VB_PROPERTY_GET         : P R O P E R T Y VB_WS G E T;
VB_PROPERTY_LET         : P R O P E R T Y VB_WS L E T;
VB_PROPERTY_SET         : P R O P E R T Y VB_WS S E T;
VB_PUBLIC               : P U B L I C;
VB_PUT                  : P U T;
VB_RANDOMIZE            : R A N D O M I Z E;
VB_RAISEEVENT           : R A I S E E V E N T;
VB_REDIM                : R E D I M;
VB_RESET                : R E S E T;
VB_RESUME               : R E S U M E;
VB_RETURN               : R E T U R N;
VB_RMDIR                : R M D I R;
VB_RSET                 : R S E T;
VB_SAVEPICTURE          : S A V E P I C T U R E;
VB_SAVESETTING          : S A V E S E T T I N G;
VB_SEEK                 : S E E K;
VB_SELECT               : S E L E C T;
VB_SENDKEYS             : S E N D K E Y S;
VB_SET                  : S E T;
VB_SETATTR              : S E T A T T R;
VB_SINGLE               : S I N G L E;
VB_SPC                  : S P C;
VB_STATIC               : S T A T I C;
VB_STEP                 : S T E P;
VB_STOP                 : S T O P;
VB_STRING               : S T R I N G;
VB_SUB                  : S U B;
VB_TAB                  : T A B;
VB_TEXT                 : T E X T;
VB_THEN                 : T H E N;
VB_TO                   : T O;
VB_TRUE                 : T R U E;
VB_TYPE                 : T Y P E;
VB_TYPEOF               : T Y P E O F;
VB_UNLOAD               : U N L O A D;
VB_UNLOCK               : U N L O C K;
VB_UNTIL                : U N T I L;
VB_VARIANT              : V A R I A N T;
VB_VERSION              : V E R S I O N;
VB_WEND                 : W E N D;
VB_WHILE                : W H I L E;
VB_WIDTH                : W I D T H;
VB_WITH                 : W I T H;
VB_WITHEVENTS           : W I T H E V E N T S;
VB_XOR                  : X O R;

// symbols

VB_AMPERSAND: '&';
VB_ASSIGN: ':=';
VB_AT: '@';
VB_COLON: ':';
VB_COMMA: ',';
VB_DIV: '\\' | '/';
VB_DOLLAR: '$';
VB_DOT: '.';
VB_EQ: '=';
VB_EXCLAMATIONMARK: '!';
VB_GEQ: '>=';
VB_GT: '>';
VB_HASH: '#';
VB_LEQ: '<=';
VB_LPAREN: '(';
VB_LT: '<';
VB_MINUS: '-';
VB_MINUS_EQ: '-=';
VB_MULT: '*';
VB_NEQ: '<>';
VB_PERCENT: '%';
VB_PLUS: '+';
VB_PLUS_EQ: '+=';
VB_POW: '^';
VB_RPAREN: ')';
VB_SEMICOLON: ';';
VB_L_SQUARE_BRACKET: '[';
VB_R_SQUARE_BRACKET: ']';

// literals

VB_STRINGLITERAL
   : '"' (~ ["\r\n] | '""')* '"'
   ;
VB_DATELITERAL
   : VB_HASH (~ [#\r\n])* VB_HASH
   ;
VB_COLORLITERAL
   : '&' H [0-9A-Fa-f] + VB_AMPERSAND?
   ;
VB_INTEGERLITERAL
   : /*(VB_PLUS | VB_MINUS)? */('0' .. '9') + (('e' | 'E') VB_INTEGERLITERAL)* (VB_HASH | VB_AMPERSAND)?
   ;
VB_DOUBLELITERAL
   : /*(VB_PLUS | VB_MINUS)? */('0' .. '9')* VB_DOT ('0' .. '9') + (('e' | 'E') (VB_PLUS | VB_MINUS)? ('0' .. '9') +)* (VB_HASH | VB_AMPERSAND)?
   ;
VB_FILENUMBER
   : VB_HASH VB_LETTERORDIGIT +
   ;
// identifier
VB_IDENTIFIER
   : VB_LETTER VB_LETTERORDIGIT*
   ;
// whitespace, line breaks, comments, ...
VB_LINE_CONTINUATION
   : VB_WS? '_' VB_WS? ('\r'? '\n' | '\r')+ -> skip
   ;
VB_NEWLINE
   : VB_WS? (('\r'? '\n' | '\r') | VB_COLON ' ') VB_WS?
   ;
VB_COMMENT_START
   : VB_WS? ('\'' | '//') ->pushMode(SingleLineCommentMode), channel(AspComments) /*(VB_LINE_CONTINUATION | ~ ('\n' | '\r'))* -> skip*/
   ;

VB_WS: [ \t] +;


// letters

fragment VB_LETTER: [a-zA-Z_];


fragment VB_LETTERORDIGIT: [a-zA-Z0-9_];

// case insensitive chars

fragment A: ('a' | 'A');
fragment B: ('b' | 'B');
fragment C: ('c' | 'C');
fragment D: ('d' | 'D');
fragment E: ('e' | 'E');
fragment F: ('f' | 'F');
fragment G: ('g' | 'G');
fragment H: ('h' | 'H');
fragment I: ('i' | 'I');
fragment J: ('j' | 'J');
fragment K: ('k' | 'K');
fragment L: ('l' | 'L');
fragment M: ('m' | 'M');
fragment N: ('n' | 'N');
fragment O: ('o' | 'O');
fragment P: ('p' | 'P');
fragment Q: ('q' | 'Q');
fragment R: ('r' | 'R');
fragment S: ('s' | 'S');
fragment T: ('t' | 'T');
fragment U: ('u' | 'U');
fragment V: ('v' | 'V');
fragment W: ('w' | 'W');
fragment X: ('x' | 'X');
fragment Y: ('y' | 'Y');
fragment Z: ('z' | 'Z');

ErrorAsp:                   .          -> channel(ErrorLexem);

mode ASPDirective;
ASPDrctvEnd: '%>' -> popMode;
ASPDrctvEQ: '=' -> type(VB_EQ);
ASPDrctvWS: [ \t] + -> type(VB_WS);
ASPDrctvNEWLINE: ASPDrctvWS? ('\r'? '\n' | ':' ' ') ASPDrctvWS? -> type(VB_NEWLINE);

ASPDrctv_LANGUAGE: L A N G U A G E;
ASPDrctv_ENABLESESSIONSTATE: E N A B L E S E S S I O N S T A T E;
ASPDrctv_CODEPAGE: C O D E P A G E;
ASPDrctv_LCID:L C I D;
ASPDrctv_TRANSACTION: T R A N S A C T I O N;

ASPDrctvVAL
   : '"' (~ ["\r\n] | '""')* '"'
   | VB_LETTERORDIGIT+
   ;

mode SingleLineCommentMode;

VB_COMMENT:                 (LineContinuation | ~[\r\n%])+ -> channel(AspComments);
ASPEndSingleLineComment: '%' '>';
CommentQuestionMark:     '%' -> type(VB_COMMENT), channel(AspComments);
VB_COMMENT_END:              '\r'? '\n' VB_WS? -> type(VB_NEWLINE), popMode; // exit from comment.

fragment LineContinuation:        VB_WS '-' '\r'? '\n';

mode VbOpenStatement;
VB_APPEND               : A P P E N D;
VB_BINARY               : B I N A R Y;
VB_INPUT                : I N P U T;
VB_OUTPUT               : O U T P U T;
VB_RANDOM               : R A N D O M;
VB_WRITE                : W R I T E;
VB_READ                 : R E A D;
VB_READ_WRITE           : R E A D ' ' W R I T E;
VB_SHARED               : S H A R E D;
VB_LOCK_READ            : L O C K ' ' R E A D;
VB_LOCK_WRITE           : L O C K ' ' W R I T E;
VB_LOCK_READ_WRITE      : L O C K ' ' R E A D ' ' W R I T E;
VB_AS                   : A S;
VB_LEN                  : L E N;
VbOpenEnd:              [\r\n] -> skip, popMode; // exit from Open statement.
VbOpenLineContinuation:        '-' '\r'? '\n' -> skip;


// fragments.
// '<%=' will be transformed to 'ASP_ECHO_START' token.
// '<%= "Hello world"; %>' will be transformed to '<% Response.Write "Hello world"; %>'
fragment AspStartEchoFragment: '<' '%' VB_WS? '=';
fragment AspStartFragment:     '<' '%';
fragment NameChar
    : NameStartChar
    | '-'
    | '_'
    | '.'
    | Digit
    | '\u00B7'
    | '\u0300'..'\u036F'
    | '\u203F'..'\u2040'
    ;
fragment NameStartChar
    : [:a-zA-Z]
    | '\u2070'..'\u218F'
    | '\u2C00'..'\u2FEF'
    | '\u3001'..'\uD7FF'
    | '\uF900'..'\uFDCF'
    | '\uFDF0'..'\uFFFD'
    ;
fragment InlineJsChar
    : ~[ ="'>]
    ;
fragment ExponentPart:         'e' [+-]? Digit+;
fragment Digit:                [0-9];
fragment HexDigit:             [a-fA-F0-9];