" Vim syntax file
" Language: Aback
" Maintainer: Jakob Kenda
" Latest Revision: 27 October 2023
" Filenames:   *.ab

if exists("b:current_syntax")
    finish
endif

" Keywords
syn keyword abackCond if then else
syn keyword abackCond take peek in
syn keyword abackCond while do
syn keyword abackCond end
syn keyword abackCond macro proc is
syn keyword abackStack drop  nip  dup  over  tuck  swap  rot  -rot
syn keyword abackStack 2drop 2nip 2dup 2over 2tuck 2swap 2rot 2-rot
syn keyword forthStack 3dup 4dup 5dup 3drop 4drop 5drop

syn keyword abackInclude include

syn keyword abackBool true false
syn keyword abackFunc puts putc puti putf putb

syn match abackOperator '+\|-\|*\|/\|%'
syn match abackOperator '=\|!=\|<\|>\|<=\|>='
syn match abackOperator '|>'

" Matches
" Integer with - + or nothing in front
syn match abackNumber '\d\+'
syn match abackNumber '[-+]\d\+'
" Floating point number with decimal no E or e 
syn match abackNumber '[-+]\d\+\.\d*'

" Regions
syn region abackIfBlock     start='then' end='end'  fold transparent
syn region abackIfElseBlock start='then' end='else' fold transparent 
syn region abackElseBlock   start='else' end='end'  fold transparent
syn region abackWhileBlock  start='do'   end='end'  fold transparent
syn region abackPeekBlock   start='in'   end='end'  fold transparent
syn region abackFuncBlock   start='is'   end='end'  fold transparent
 
syn region abackString start='"' end='"'
syn region abackChar   start="'" end="'"

let b:current_syntax = "aback"

hi def link abackString   String
hi def link abackChar     Character
hi def link abackNumber   Number
hi def link abackBool     Boolean
hi def link abackOperator Operator
hi def link abackCond     Conditional
hi def link abackFunc     Function
hi def link abackStack    Macro
hi def link abackInclude  Include

