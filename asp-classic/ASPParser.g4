/*
PHP grammar.
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

parser grammar ASPParser;

options { tokenVocab=ASPLexer; }

// HTML
// Also see here: https://github.com/antlr/grammars-v4/tree/master/html

htmlDocument
    : htmlElementOrAspBlock* EOF
    ;

htmlElementOrAspBlock
    : htmlElements
    | aspDirectiveBlock
    | aspBlock
    | aspEchoBlock
    | scriptTextPart
    | styleTextPart
    ;

htmlElements
    : htmlElement+
    ;

htmlElement
    : HtmlDtd
    | HtmlScriptOpen
    | HtmlClose
    | HtmlStyleOpen
    | ScriptClose
    | StyleClose
    | HtmlOpen
    | HtmlName
    | HtmlSlashClose
    | HtmlSlash
    | HtmlText 
    | HtmlEquals
    | HtmlStartQuoteString
    | HtmlEndQuoteString
    | HtmlStartDoubleQuoteString
    | HtmlEndDoubleQuoteString
    | HtmlAttrName
    | HtmlAttrVal
    | HtmlEventName
    | HtmlEventVal
    | HtmlEndTag
    | SeaWhitespace
    | HtmlSpace
    | HtmlComment
    ;

// Script
// Parse JavaScript with https://github.com/antlr/grammars-v4/tree/master/ecmascript if necessary.
    
scriptTextPart
    : ScriptText+
    ;

styleTextPart
    : StyleText+
    ;

// ASP block (Visual Basic 6)

aspDirectiveBlock
   : ASPStartDirective VB_NEWLINE* VB_WS? (aspDirectiveStmt ((VB_NEWLINE+ | VB_WS) aspDirectiveStmt)*)? VB_NEWLINE* VB_WS? ASPDrctvEnd
   ;

aspDirectiveStmt
   : aspDirective VB_WS? VB_EQ VB_WS? aspDirectiveValue
   ;

aspDirective
   : ASPDrctv_LANGUAGE
   | ASPDrctv_ENABLESESSIONSTATE
   | ASPDrctv_CODEPAGE
   | ASPDrctv_LCID
   | ASPDrctv_TRANSACTION
   ;

aspDirectiveValue: ASPDrctvVAL;

aspBlock
   : ASPStart VB_WS? VB_NEWLINE* (moduleHeader VB_NEWLINE +)? moduleConfig? VB_NEWLINE* moduleAttributes? VB_NEWLINE* moduleOptions? VB_NEWLINE* (moduleBody (VB_NEWLINE+ moduleBody)*)? VB_NEWLINE* VB_WS? ASPEnd
   ;

aspEchoBlock
   : ASPStartEcho VB_WS? VB_NEWLINE* VB_WS? valueStmt? VB_WS? VB_NEWLINE* VB_WS? ASPEnd
   ;

moduleHeader
   : VB_VERSION VB_WS VB_DOUBLELITERAL VB_WS VB_CLASS
   ;

moduleConfig
   : VB_BEGIN VB_NEWLINE + moduleConfigElement + VB_END VB_NEWLINE +
   ;

moduleConfigElement
   : ambiguousIdentifier VB_WS? VB_EQ VB_WS? literal VB_NEWLINE
   ;

moduleAttributes
   : (attributeStmt VB_NEWLINE +) +
   ;

moduleOptions
   : (moduleOption VB_NEWLINE +) +
   ;

moduleOption
   : VB_OPTION_BASE VB_WS VB_INTEGERLITERAL # optionBaseStmt
   | VB_OPTION_COMPARE VB_WS (VB_BINARY | VB_TEXT) # optionCompareStmt
   | VB_OPTION_EXPLICIT # optionExplicitStmt
   | VB_OPTION_PRIVATE_MODULE # optionPrivateModuleStmt
   ;

moduleBody
   : moduleBodyElement (VB_NEWLINE + moduleBodyElement)*
   ;

moduleBodyElement
   : moduleBlock
   | declareStmt
   | enumerationStmt
   | eventStmt
   | functionStmt
   | macroIfThenElseStmt
   | propertyGetStmt
   | propertySetStmt
   | propertyLetStmt
   | subStmt
   | typeStmt
   ;

// block ----------------------------------
moduleBlock
   : block
   ;

attributeStmt
   : VB_ATTRIBUTE VB_WS implicitCallStmt_InStmt VB_WS? VB_EQ VB_WS? literal (VB_WS? VB_COMMA VB_WS? literal)*
   ;

block
   : blockStmt (VB_NEWLINE + blockStmt)*
   ;

echoableBlock
    /*: echoableBlockStmt ((VB_NEWLINE+ | aspEchoStmt VB_NEWLINE*) echoableBlockStmt)* */ // This rule cannot parse "SomeVbStmt %> ... <%\t\t\tSomeVbStmt"
   : (blockStmt VB_NEWLINE+ | aspEchoStmt VB_NEWLINE*)+
   ;

echoableBlockStmt
   : blockStmt
   | aspEchoStmt
   ;

aspEchoStmt
   : VB_WS? ASPEnd (aspBlock*? (htmlElements | scriptTextPart | aspEchoBlock))*? ASPStart VB_WS? // %> ~ <% Outside of Asp block will be recognized as html block
   //| VB_WS? ASPEnd (htmlElement | scriptTextPart | aspEchoBlock)* ASPStart VB_WS?
   ;

blockStmt
   : appactivateStmt
   | attributeStmt
   | beepStmt
   | chdirStmt
   | chdriveStmt
   | closeStmt
   | constStmt
   | dateStmt
   | deleteSettingStmt
   | deftypeStmt
   | doLoopStmt
   | endStmt
   | eraseStmt
   | errorStmt
   | exitStmt
   | explicitCallStmt
   | filecopyStmt
   | forEachStmt
   | forNextStmt
   | getStmt
   | goSubStmt
   | goToStmt
   | ifThenElseStmt
   | implementsStmt
   | inputStmt
   | killStmt
   | letStmt
   | lineInputStmt
   | lineLabel
   | lockStmt
   | lsetStmt
   | macroIfThenElseStmt
   | mkdirStmt
   | nameStmt
   | onErrorStmt
   | onGoToStmt
   | onGoSubStmt
   | openStmt
   | printStmt
   | putStmt
   | raiseEventStmt
   | randomizeStmt
   | redimStmt
   | resetStmt
   | resumeStmt
   | returnStmt
   | rmdirStmt
   | rsetStmt
   | savepictureStmt
   | saveSettingStmt
   | seekStmt
   | selectCaseStmt
   | sendkeysStmt
   | setattrStmt
   | setStmt
   | stopStmt
   | unlockStmt
   | variableStmt
   | whileWendStmt
   | widthStmt
   | withStmt
   | writeStmt
   | implicitCallStmt_InBlock
   | implicitCallStmt_InStmt
   ;

// statements ----------------------------------
appactivateStmt
   : VB_APPACTIVATE VB_WS valueStmt (VB_WS? VB_COMMA VB_WS? valueStmt)?
   ;

beepStmt
   : VB_BEEP
   ;

chdirStmt
   : VB_CHDIR VB_WS valueStmt
   ;

chdriveStmt
   : VB_CHDRIVE VB_WS valueStmt
   ;

closeStmt
   : VB_CLOSE (VB_WS valueStmt (VB_WS? VB_COMMA VB_WS? valueStmt)*)?
   ;

constStmt
   : (visibility VB_WS)? VB_CONST VB_WS constSubStmt (VB_WS? VB_COMMA VB_WS? constSubStmt)*
   ;

constSubStmt
   : ambiguousIdentifier typeHint? (VB_WS asTypeClause)? VB_WS? VB_EQ VB_WS? valueStmt
   ;

dateStmt
   : VB_DATE VB_WS? VB_EQ VB_WS? valueStmt
   ;

declareStmt
   : (visibility VB_WS)? VB_DECLARE VB_WS (VB_FUNCTION typeHint? | VB_SUB) VB_WS ambiguousIdentifier typeHint? VB_WS VB_LIB VB_WS VB_STRINGLITERAL (VB_WS VB_ALIAS VB_WS VB_STRINGLITERAL)? (VB_WS? argList)? (VB_WS asTypeClause)?
   ;

deftypeStmt
   : (VB_DEFBOOL | VB_DEFBYTE | VB_DEFINT | VB_DEFLNG | VB_DEFCUR | VB_DEFSNG | VB_DEFDBL | VB_DEFDEC | VB_DEFDATE | VB_DEFSTR | VB_DEFOBJ | VB_DEFVAR) VB_WS letterrange (VB_WS? VB_COMMA VB_WS? letterrange)*
   ;

deleteSettingStmt
   : VB_DELETESETTING VB_WS valueStmt VB_WS? VB_COMMA VB_WS? valueStmt (VB_WS? VB_COMMA VB_WS? valueStmt)?
   ;

doLoopStmt
   : VB_DO (VB_NEWLINE + echoableBlock? | aspEchoStmt)+ VB_LOOP
   | VB_DO VB_WS (VB_WHILE | VB_UNTIL) VB_WS valueStmt (VB_NEWLINE + echoableBlock? | aspEchoStmt (VB_NEWLINE* echoableBlock)?)+ VB_LOOP
   | VB_DO (VB_NEWLINE + echoableBlock? | aspEchoStmt)+ VB_LOOP VB_WS (VB_WHILE | VB_UNTIL) VB_WS valueStmt
   ;

endStmt
   : VB_END
   ;

enumerationStmt
   : (visibility VB_WS)? VB_ENUM VB_WS ambiguousIdentifier VB_NEWLINE + (enumerationStmt_Constant)* VB_END_ENUM
   ;

enumerationStmt_Constant
   : ambiguousIdentifier (VB_WS? VB_EQ VB_WS? valueStmt)? VB_NEWLINE +
   ;

eraseStmt
   : VB_ERASE VB_WS valueStmt (VB_WS? VB_COMMA VB_WS? valueStmt)*
   ;

errorStmt
   : VB_ERROR VB_WS valueStmt
   ;

eventStmt
   : (visibility VB_WS)? VB_EVENT VB_WS ambiguousIdentifier VB_WS? argList
   ;

exitStmt
   : VB_EXIT_DO
   | VB_EXIT_FOR
   | VB_EXIT_FUNCTION
   | VB_EXIT_PROPERTY
   | VB_EXIT_SUB
   ;

filecopyStmt
   : VB_FILECOPY VB_WS valueStmt VB_WS? VB_COMMA VB_WS? valueStmt
   ;

forEachStmt
   : VB_FOR VB_WS VB_EACH VB_WS ambiguousIdentifier typeHint? VB_WS VB_IN VB_WS valueStmt (VB_NEWLINE + echoableBlock? | aspEchoStmt (VB_NEWLINE* echoableBlock)?)+ VB_NEXT (VB_WS ambiguousIdentifier)?
   ;

forNextStmt
   : VB_FOR VB_WS ambiguousIdentifier typeHint? (VB_WS asTypeClause)? VB_WS? VB_EQ VB_WS? valueStmt VB_WS VB_TO VB_WS valueStmt (VB_WS VB_STEP VB_WS valueStmt)? (VB_NEWLINE + echoableBlock? | aspEchoStmt (VB_NEWLINE* echoableBlock)?)+ VB_NEXT (VB_WS ambiguousIdentifier)?
   ;

functionStmt
   : (visibility VB_WS)? (VB_STATIC VB_WS)? VB_FUNCTION VB_WS ambiguousIdentifier (VB_WS? argList)? (VB_WS asTypeClause)? (VB_NEWLINE + echoableBlock? | aspEchoStmt (VB_NEWLINE* echoableBlock)?)+ VB_END_FUNCTION
   ;

getStmt
   : VB_GET VB_WS valueStmt VB_WS? VB_COMMA VB_WS? valueStmt? VB_WS? VB_COMMA VB_WS? valueStmt
   ;

goSubStmt
   : VB_GOSUB VB_WS valueStmt
   ;

goToStmt
   : VB_GOTO VB_WS valueStmt
   ;

ifThenElseStmt
   : VB_IF (VB_WS ifConditionStmt | VB_WS? VB_LPAREN VB_WS? ifConditionStmt VB_WS? VB_RPAREN) VB_WS? VB_THEN VB_WS blockStmt (VB_WS VB_ELSE VB_WS blockStmt)? (VB_WS VB_END_IF)? # inlineIfThenElse
   | ifBlockStmt ifElseIfBlockStmt* ifElseBlockStmt? VB_END_IF # blockIfThenElse
   ;

ifBlockStmt
   : VB_IF (VB_WS ifConditionStmt | VB_WS? VB_LPAREN VB_WS? ifConditionStmt VB_WS? VB_RPAREN) VB_WS? VB_THEN (VB_NEWLINE + echoableBlock? | aspEchoStmt (VB_NEWLINE* echoableBlock)?)+
   ;

ifConditionStmt
   : valueStmt
   ;

ifElseIfBlockStmt
   : VB_ELSEIF (VB_WS ifConditionStmt | VB_WS? VB_LPAREN VB_WS? ifConditionStmt VB_WS? VB_RPAREN) VB_WS? VB_THEN (VB_NEWLINE + echoableBlock? | aspEchoStmt (VB_NEWLINE* echoableBlock)?)+
   ;

ifElseBlockStmt
   : VB_ELSE (VB_NEWLINE + echoableBlock? | aspEchoStmt)+
   | VB_ELSE VB_WS blockStmt VB_NEWLINE
   ;

implementsStmt
   : VB_IMPLEMENTS VB_WS ambiguousIdentifier
   ;

inputStmt
   : VB_INPUT VB_WS valueStmt (VB_WS? VB_COMMA VB_WS? valueStmt) +
   ;

killStmt
   : VB_KILL VB_WS valueStmt
   ;

letStmt
   : (VB_LET VB_WS)? implicitCallStmt_InStmt VB_WS? (VB_EQ | VB_PLUS_EQ | VB_MINUS_EQ) VB_WS? valueStmt
   ;

lineInputStmt
   : VB_LINE_INPUT VB_WS valueStmt VB_WS? VB_COMMA VB_WS? valueStmt
   ;

loadStmt
   : VB_LOAD VB_WS valueStmt
   ;

lockStmt
   : VB_LOCK VB_WS valueStmt (VB_WS? VB_COMMA VB_WS? valueStmt (VB_WS VB_TO VB_WS valueStmt)?)?
   ;

lsetStmt
   : VB_LSET VB_WS implicitCallStmt_InStmt VB_WS? VB_EQ VB_WS? valueStmt
   ;

macroIfThenElseStmt
   : macroIfBlockStmt macroElseIfBlockStmt* macroElseBlockStmt? VB_MACRO_END_IF
   ;

macroIfBlockStmt
   : VB_MACRO_IF VB_WS ifConditionStmt VB_WS VB_THEN VB_NEWLINE + (moduleBody VB_NEWLINE +)?
   ;

macroElseIfBlockStmt
   : VB_MACRO_ELSEIF VB_WS ifConditionStmt VB_WS VB_THEN VB_NEWLINE + (moduleBody VB_NEWLINE +)?
   ;

macroElseBlockStmt
   : VB_MACRO_ELSE VB_NEWLINE + (moduleBody VB_NEWLINE +)?
   ;

midStmt
   : VB_MID VB_WS? VB_LPAREN VB_WS? argsCall VB_WS? VB_RPAREN VB_WS? VB_EQ VB_WS? valueStmt
   ;

mkdirStmt
   : VB_MKDIR VB_WS valueStmt
   ;

nameStmt
   : VB_NAME VB_WS valueStmt VB_WS VB_AS VB_WS valueStmt
   ;

onErrorStmt
   : VB_ON_ERROR VB_WS (VB_GOTO VB_WS valueStmt | VB_RESUME VB_WS VB_NEXT)
   ;

onGoToStmt
   : VB_ON VB_WS valueStmt VB_WS VB_GOTO VB_WS valueStmt (VB_WS? VB_COMMA VB_WS? valueStmt)*
   ;

onGoSubStmt
   : VB_ON VB_WS valueStmt VB_WS VB_GOSUB VB_WS valueStmt (VB_WS? VB_COMMA VB_WS? valueStmt)*
   ;

openStmt
   : VB_OPEN VB_WS valueStmt VB_WS VB_FOR VB_WS (VB_APPEND | VB_BINARY | VB_INPUT | VB_OUTPUT | VB_RANDOM) (VB_WS VB_ACCESS VB_WS (VB_READ | VB_WRITE | VB_READ_WRITE))? (VB_WS (VB_SHARED | VB_LOCK_READ | VB_LOCK_WRITE | VB_LOCK_READ_WRITE))? VB_WS VB_AS VB_WS valueStmt (VB_WS VB_LEN VB_WS? VB_EQ VB_WS? valueStmt)?
   ;

outputList
   : outputList_Expression (VB_WS? (VB_SEMICOLON | VB_COMMA) VB_WS? outputList_Expression?)*
   | outputList_Expression? (VB_WS? (VB_SEMICOLON | VB_COMMA) VB_WS? outputList_Expression?) +
   ;

outputList_Expression
   : valueStmt
   | (VB_SPC | VB_TAB) (VB_WS? VB_LPAREN VB_WS? argsCall VB_WS? VB_RPAREN)?
   ;

printStmt
   : VB_PRINT VB_WS valueStmt VB_WS? VB_COMMA (VB_WS? outputList)?
   ;

propertyGetStmt
   : (visibility VB_WS)? (VB_STATIC VB_WS)? VB_PROPERTY_GET VB_WS ambiguousIdentifier typeHint? (VB_WS? argList)? (VB_WS asTypeClause)? VB_NEWLINE + (block VB_NEWLINE +)? VB_END_PROPERTY
   ;

propertySetStmt
   : (visibility VB_WS)? (VB_STATIC VB_WS)? VB_PROPERTY_SET VB_WS ambiguousIdentifier (VB_WS? argList)? VB_NEWLINE + (block VB_NEWLINE +)? VB_END_PROPERTY
   ;

propertyLetStmt
   : (visibility VB_WS)? (VB_STATIC VB_WS)? VB_PROPERTY_LET VB_WS ambiguousIdentifier (VB_WS? argList)? VB_NEWLINE + (block VB_NEWLINE +)? VB_END_PROPERTY
   ;

putStmt
   : VB_PUT VB_WS valueStmt VB_WS? VB_COMMA VB_WS? valueStmt? VB_WS? VB_COMMA VB_WS? valueStmt
   ;

raiseEventStmt
   : VB_RAISEEVENT VB_WS ambiguousIdentifier (VB_WS? VB_LPAREN VB_WS? (argsCall VB_WS?)? VB_RPAREN)?
   ;

randomizeStmt
   : VB_RANDOMIZE (VB_WS valueStmt)?
   ;

redimStmt
   : VB_REDIM VB_WS (VB_PRESERVE VB_WS)? redimSubStmt (VB_WS? VB_COMMA VB_WS? redimSubStmt)*
   ;

redimSubStmt
   : implicitCallStmt_InStmt VB_WS? VB_LPAREN VB_WS? subscripts VB_WS? VB_RPAREN (VB_WS asTypeClause)?
   ;

resetStmt
   : VB_RESET
   ;

resumeStmt
   : VB_RESUME (VB_WS (VB_NEXT | ambiguousIdentifier))?
   ;

returnStmt
   : VB_RETURN
   ;

rmdirStmt
   : VB_RMDIR VB_WS valueStmt
   ;

rsetStmt
   : VB_RSET VB_WS implicitCallStmt_InStmt VB_WS? VB_EQ VB_WS? valueStmt
   ;

savepictureStmt
   : VB_SAVEPICTURE VB_WS valueStmt VB_WS? VB_COMMA VB_WS? valueStmt
   ;

saveSettingStmt
   : VB_SAVESETTING VB_WS valueStmt VB_WS? VB_COMMA VB_WS? valueStmt VB_WS? VB_COMMA VB_WS? valueStmt VB_WS? VB_COMMA VB_WS? valueStmt
   ;

seekStmt
   : VB_SEEK VB_WS valueStmt VB_WS? VB_COMMA VB_WS? valueStmt
   ;

selectCaseStmt
   : VB_SELECT VB_WS VB_CASE VB_WS valueStmt VB_NEWLINE + sC_Case* VB_WS? VB_END_SELECT
   ;

sC_Case
   : VB_CASE VB_WS sC_Cond VB_WS? (VB_COLON? VB_NEWLINE* VB_WS? | VB_NEWLINE +) echoableBlock?
   ;

// VB_ELSE first, so that it is not interpreted as a variable call
sC_Cond
   : VB_ELSE # caseCondElse
   | VB_IS VB_WS? comparisonOperator VB_WS? valueStmt # caseCondIs
   | valueStmt (VB_WS? VB_COMMA VB_WS? valueStmt)* # caseCondValue
   | VB_INTEGERLITERAL VB_WS VB_TO VB_WS valueStmt (VB_WS? VB_COMMA VB_WS? valueStmt)* # caseCondTo
   ;

sendkeysStmt
   : VB_SENDKEYS VB_WS valueStmt (VB_WS? VB_COMMA VB_WS? valueStmt)?
   ;

setattrStmt
   : VB_SETATTR VB_WS valueStmt VB_WS? VB_COMMA VB_WS? valueStmt
   ;

setStmt
   : VB_SET VB_WS implicitCallStmt_InStmt VB_WS? VB_EQ VB_WS? valueStmt
   ;

stopStmt
   : VB_STOP
   ;

subStmt
   : (visibility VB_WS)? (VB_STATIC VB_WS)? VB_SUB VB_WS ambiguousIdentifier (VB_WS? argList)? (VB_NEWLINE + echoableBlock? | aspEchoStmt (VB_NEWLINE* echoableBlock)?)+ VB_END_SUB
   ;

typeStmt
   : (visibility VB_WS)? VB_TYPE VB_WS ambiguousIdentifier VB_NEWLINE + (typeStmt_Element)* VB_END_TYPE
   ;

typeStmt_Element
   : ambiguousIdentifier (VB_WS? VB_LPAREN (VB_WS? subscripts)? VB_WS? VB_RPAREN)? (VB_WS asTypeClause)? VB_NEWLINE +
   ;

typeOfStmt
   : VB_TYPEOF VB_WS valueStmt (VB_WS VB_IS VB_WS type)?
   ;

unloadStmt
   : VB_UNLOAD VB_WS valueStmt
   ;

unlockStmt
   : VB_UNLOCK VB_WS valueStmt (VB_WS? VB_COMMA VB_WS? valueStmt (VB_WS VB_TO VB_WS valueStmt)?)?
   ;

// operator precedence is represented by rule order
valueStmt
   : literal # vsLiteral
   | implicitCallStmt_InStmt # vsICS
   | VB_LPAREN VB_WS? valueStmt (VB_WS? VB_COMMA VB_WS? valueStmt)* VB_WS? VB_RPAREN # vsStruct
   | VB_NEW VB_WS valueStmt # vsNew
   | typeOfStmt # vsTypeOf
   | VB_ADDRESSOF VB_WS valueStmt # vsAddressOf
   | implicitCallStmt_InStmt VB_WS? VB_ASSIGN VB_WS? valueStmt # vsAssign
   | valueStmt VB_WS VB_IS VB_WS valueStmt # vsIs
   | valueStmt VB_WS VB_LIKE VB_WS valueStmt # vsLike
   | valueStmt VB_WS? VB_GEQ VB_WS? valueStmt # vsGeq
   | valueStmt VB_WS? VB_LEQ VB_WS? valueStmt # vsLeq
   | valueStmt VB_WS? VB_GT VB_WS? valueStmt # vsGt
   | valueStmt VB_WS? VB_LT VB_WS? valueStmt # vsLt
   | valueStmt VB_WS? VB_NEQ VB_WS? valueStmt # vsNeq
   | valueStmt VB_WS? VB_EQ VB_WS? valueStmt # vsEq
   | valueStmt VB_WS? VB_AMPERSAND VB_WS? valueStmt # vsAmp
   | VB_MINUS VB_WS? valueStmt # vsNegation
   | VB_PLUS VB_WS? valueStmt # vsPlus
   | valueStmt VB_WS? VB_PLUS VB_WS? valueStmt # vsAdd
   | valueStmt VB_WS? VB_MOD VB_WS? valueStmt # vsMod
   | valueStmt VB_WS? VB_DIV VB_WS? valueStmt # vsDiv
   | valueStmt VB_WS? VB_MULT VB_WS? valueStmt # vsMult
   | valueStmt VB_WS? VB_MINUS VB_WS? valueStmt # vsMinus
   | valueStmt VB_WS? VB_POW VB_WS? valueStmt # vsPow
   | valueStmt VB_WS VB_IMP VB_WS valueStmt # vsImp
   | valueStmt VB_WS VB_EQV VB_WS valueStmt # vsEqv
   | valueStmt VB_WS? VB_XOR VB_WS? valueStmt # vsXor
   | valueStmt VB_WS? VB_OR VB_WS? valueStmt # vsOr
   | valueStmt VB_WS VB_AND VB_WS valueStmt # vsAnd
   | VB_NOT VB_WS valueStmt # vsNot
   | VB_NOT VB_WS? VB_LPAREN VB_WS? valueStmt VB_WS? VB_RPAREN # vsNot
   ;

variableStmt
   : (VB_DIM | VB_STATIC | visibility) VB_WS (VB_WITHEVENTS VB_WS)? variableListStmt
   ;

variableListStmt
   : variableSubStmt (VB_WS? VB_COMMA VB_WS? variableSubStmt)*
   ;

variableSubStmt
   : ambiguousIdentifier (VB_WS? VB_LPAREN VB_WS? (subscripts VB_WS?)? VB_RPAREN VB_WS?)? typeHint? (VB_WS asTypeClause)?
   ;

whileWendStmt
   : VB_WHILE VB_WS valueStmt (VB_NEWLINE + echoableBlock* | aspEchoStmt (VB_NEWLINE* echoableBlock)?)+ VB_WEND
   ;

widthStmt
   : VB_WIDTH VB_WS valueStmt VB_WS? VB_COMMA VB_WS? valueStmt
   ;

withStmt
   : VB_WITH VB_WS implicitCallStmt_InStmt VB_NEWLINE + (block VB_NEWLINE +)? VB_END_WITH
   ;

writeStmt
   : VB_WRITE VB_WS valueStmt VB_WS? VB_COMMA (VB_WS? outputList)?
   ;

// complex call statements ----------------------------------
explicitCallStmt
   : eCS_ProcedureCall
   | eCS_MemberProcedureCall
   ;

// parantheses are required in case of args -> empty parantheses are removed
eCS_ProcedureCall
   : VB_CALL VB_WS ambiguousIdentifier typeHint? (VB_WS? VB_LPAREN VB_WS? argsCall? VB_WS? VB_RPAREN)?
   ;

// parantheses are required in case of args -> empty parantheses are removed
eCS_MemberProcedureCall
   : VB_CALL VB_WS implicitCallStmt_InStmt? VB_DOT ambiguousIdentifier typeHint? (VB_WS? VB_LPAREN VB_WS? argsCall? VB_WS? VB_RPAREN)?
   ;

// MEMO
// Difference between:
//   implicitCallStmt_InBlock
//   implicitCallStmt_InStmt
// is whether parenthesis exists?

implicitCallStmt_InBlock
   : iCS_B_ProcedureCall
   | iCS_B_MemberProcedureCall
   ;

// parantheses are forbidden in case of args
// variables cannot be called in blocks
// certainIdentifier instead of ambiguousIdentifier for preventing ambiguity with statement keywords 
iCS_B_ProcedureCall
   : certainIdentifier (VB_WS argsCall)?
   ;

iCS_B_MemberProcedureCall
   : (iCS_S_VariableOrProcedureCall | iCS_S_ProcedureOrArrayCall)? iCS_S_MemberCall + typeHint? (VB_WS argsCall)? dictionaryCallStmt?
   ;

// iCS_S_MembersCall first, so that member calls are not resolved as separate iCS_S_VariableOrProcedureCalls
implicitCallStmt_InStmt
   : iCS_S_MembersCall
   | iCS_S_VariableOrProcedureCall
   | iCS_S_ProcedureOrArrayCall
   | iCS_S_DictionaryCall
   | VB_LPAREN VB_WS? implicitCallStmt_InStmt VB_WS? VB_RPAREN iCS_S_ProcedureOrArrayNestArgs*
   ;

iCS_S_VariableOrProcedureCall
   : certainIdentifier typeHint? dictionaryCallStmt?
   ;

iCS_S_ProcedureOrArrayCall
   : (certainIdentifier | baseType) typeHint? iCS_S_ProcedureOrArrayNestArgs dictionaryCallStmt?
   ;

iCS_S_ProcedureOrArrayNestArgs
   : VB_WS? VB_LPAREN VB_WS? argsCall? VB_WS? VB_RPAREN
   | VB_WS? VB_LPAREN VB_WS? argsCall? VB_WS? VB_RPAREN iCS_S_ProcedureOrArrayNestArgs+
   ;

iCS_S_MembersCall
   : (iCS_S_VariableOrProcedureCall | iCS_S_ProcedureOrArrayCall)? iCS_S_MemberCall + dictionaryCallStmt?
   ;

iCS_S_MemberCall
   : VB_DOT (iCS_S_VariableOrProcedureCall | iCS_S_ProcedureOrArrayCall)
   ;

iCS_S_DictionaryCall
   : dictionaryCallStmt
   ;

// atomic call statements ----------------------------------
argsCall
   : (argCall? VB_WS? (VB_COMMA | VB_SEMICOLON) VB_WS?)* argCall (VB_WS? (VB_COMMA | VB_SEMICOLON) VB_WS? argCall?)*
   ;

argCall
   : ((VB_BYVAL | VB_BYREF | VB_PARAMARRAY) VB_WS)? valueStmt
   ;

dictionaryCallStmt
   : VB_EXCLAMATIONMARK ambiguousIdentifier typeHint?
   ;

// atomic rules for statements
argList
   : VB_LPAREN (VB_WS? arg (VB_WS? VB_COMMA VB_WS? arg)*)? VB_WS? VB_RPAREN
   ;

arg
   : (VB_OPTIONAL VB_WS)? ((VB_BYVAL | VB_BYREF) VB_WS)? (VB_PARAMARRAY VB_WS)? ambiguousIdentifier (VB_WS? VB_LPAREN VB_WS? VB_RPAREN)? (VB_WS asTypeClause)? (VB_WS? argDefaultValue)?
   ;

argDefaultValue
   : VB_EQ VB_WS? (literal | ambiguousIdentifier)
   ;

subscripts
   : subscript (VB_WS? VB_COMMA VB_WS? subscript)*
   ;

subscript
   : (valueStmt VB_WS VB_TO VB_WS)? valueStmt
   ;

// atomic rules ----------------------------------
ambiguousIdentifier
   : (VB_IDENTIFIER | ambiguousKeyword) +
   | VB_L_SQUARE_BRACKET (VB_IDENTIFIER | ambiguousKeyword) + VB_R_SQUARE_BRACKET
   ;

asTypeClause
   : VB_AS VB_WS (VB_NEW VB_WS)? type (VB_WS fieldLength)?
   ;

baseType
   : VB_BOOLEAN
   | VB_BYTE
   | VB_COLLECTION
   | VB_DATE
   | VB_DOUBLE
   | VB_INTEGER
   | VB_LONG
   | VB_SINGLE
   | VB_STRING
   | VB_VARIANT
   ;

certainIdentifier
   : VB_IDENTIFIER (ambiguousKeyword | VB_IDENTIFIER)*
   | ambiguousKeyword (ambiguousKeyword | VB_IDENTIFIER) +
   ;

comparisonOperator
   : VB_LT
   | VB_LEQ
   | VB_GT
   | VB_GEQ
   | VB_EQ
   | VB_NEQ
   | VB_IS
   | VB_LIKE
   ;

complexType
   : ambiguousIdentifier (VB_DOT ambiguousIdentifier)*
   ;

fieldLength
   : VB_MULT VB_WS? (VB_INTEGERLITERAL | ambiguousIdentifier)
   ;

letterrange
   : certainIdentifier (VB_WS? VB_MINUS VB_WS? certainIdentifier)?
   ;

lineLabel
   : ambiguousIdentifier VB_COLON
   ;

literal
   : VB_COLORLITERAL
   | VB_DATELITERAL
   | vbDoubleLiteral
   | VB_FILENUMBER
   | vbIntegerLiteral
   | VB_STRINGLITERAL
   | VB_TRUE
   | VB_FALSE
   | VB_NOTHING
   | VB_NULL
   ;

vbIntegerLiteral
   : (VB_PLUS | VB_MINUS)? VB_INTEGERLITERAL
   ;

vbDoubleLiteral
   : (VB_PLUS | VB_MINUS)? VB_DOUBLELITERAL
   ;

type
   : (baseType | complexType) (VB_WS? VB_LPAREN VB_WS? VB_RPAREN)?
   ;

typeHint
   : VB_AMPERSAND
   | VB_AT
   | VB_DOLLAR
   | VB_EXCLAMATIONMARK
   | VB_HASH
   | VB_PERCENT
   ;

visibility
   : VB_PRIVATE
   | VB_PUBLIC
   | VB_FRIEND
   | VB_GLOBAL
   ;

// ambiguous keywords
ambiguousKeyword
   : VB_ACCESS
   | VB_ADDRESSOF
   | VB_ALIAS
   | VB_AND
   | VB_ATTRIBUTE
   | VB_APPACTIVATE
   | VB_APPEND
   | VB_AS
   | VB_BEEP
   | VB_BEGIN
   | VB_BINARY
   | VB_BOOLEAN
   | VB_BYVAL
   | VB_BYREF
   | VB_BYTE
   | VB_CALL
   | VB_CASE
   | VB_CLASS
   | VB_CLOSE
   | VB_CHDIR
   | VB_CHDRIVE
   | VB_COLLECTION
   | VB_CONST
   | VB_DATE
   | VB_DECLARE
   | VB_DEFBOOL
   | VB_DEFBYTE
   | VB_DEFCUR
   | VB_DEFDBL
   | VB_DEFDATE
   | VB_DEFDEC
   | VB_DEFINT
   | VB_DEFLNG
   | VB_DEFOBJ
   | VB_DEFSNG
   | VB_DEFSTR
   | VB_DEFVAR
   | VB_DELETESETTING
   | VB_DIM
   | VB_DO
   | VB_DOUBLE
   | VB_EACH
   | VB_ELSE
   | VB_ELSEIF
   | VB_END
   | VB_ENUM
   | VB_EQV
   | VB_ERASE
   | VB_ERROR
   | VB_EVENT
   | VB_FALSE
   | VB_FILECOPY
   | VB_FRIEND
   | VB_FOR
   | VB_FUNCTION
   | VB_GET
   | VB_GLOBAL
   | VB_GOSUB
   | VB_GOTO
   | VB_IF
   | VB_IMP
   | VB_IMPLEMENTS
   | VB_IN
   | VB_INPUT
   | VB_IS
   | VB_INTEGER
   | VB_KILL
   | VB_LOCK
   | VB_LONG
   | VB_LOOP
   | VB_LEN
   | VB_LET
   | VB_LIB
   | VB_LIKE
   | VB_LSET
   | VB_ME
   | VB_MKDIR
   | VB_MOD
   | VB_NAME
   | VB_NEXT
   | VB_NEW
   | VB_NOT
   | VB_NOTHING
   | VB_NULL
   | VB_ON
   | VB_OPEN
   | VB_OPTIONAL
   | VB_OR
   | VB_OUTPUT
   | VB_PARAMARRAY
   | VB_PRESERVE
   | VB_PRINT
   | VB_PRIVATE
   | VB_PUBLIC
   | VB_PUT
   | VB_RANDOM
   | VB_RANDOMIZE
   | VB_RAISEEVENT
   | VB_READ
   | VB_REDIM
   | VB_RESET
   | VB_RESUME
   | VB_RETURN
   | VB_RMDIR
   | VB_RSET
   | VB_SAVEPICTURE
   | VB_SAVESETTING
   | VB_SEEK
   | VB_SELECT
   | VB_SENDKEYS
   | VB_SET
   | VB_SETATTR
   | VB_SHARED
   | VB_SINGLE
   | VB_SPC
   | VB_STATIC
   | VB_STEP
   | VB_STOP
   | VB_STRING
   | VB_SUB
   | VB_TAB
   | VB_TEXT
   | VB_THEN
   | VB_TO
   | VB_TRUE
   | VB_TYPE
   | VB_TYPEOF
   | VB_UNLOCK
   | VB_UNTIL
   | VB_VARIANT
   | VB_VERSION
   | VB_WEND
   | VB_WHILE
   | VB_WIDTH
   | VB_WITH
   | VB_WITHEVENTS
   | VB_WRITE
   | VB_XOR
;