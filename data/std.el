___SET___ setType setType ___"BLOCK___ pattern typeName ___BLOCK"___ ___"BLOCK___ ___SET___ pattern typeName ___"BLOCK___ ___BLOCK"___ ___"BLOCK___ ___BLOCK"___ ___BLOCK"___
___ALIAS___ alias ___ALIAS___

alias set  ___SET___
alias type ___TYPE___
alias self ___SELF___
alias {    ___"BLOCK___
alias }    ___BLOCK"___
alias \(   ___(BLOCK___
alias \)   ___BLOCK)___
alias const ___CONST___

arithmetic operations for floats
set -?[0-9]*\.?[0-9]+ float { type + const type arg2 float }  { ___ADD___ self arg2 }
set -?[0-9]*\.?[0-9]+ float { type + const type arg2 int }    { ___ADD___ self arg2 }
set -?[0-9]*\.?[0-9]+ float { type - const type arg2 float }  { ___SUB___ self arg2 }
set -?[0-9]*\.?[0-9]+ float { type - const type arg2 int }    { ___SUB___ self arg2 }
set -?[0-9]*\.?[0-9]+ float { type * const type arg2 float }  { ___MUL___ self arg2 }
set -?[0-9]*\.?[0-9]+ float { type * const type arg2 int }    { ___MUL___ self arg2 }
set -?[0-9]*\.?[0-9]+ float { type / const type arg2 float }  { ___DIV___ self arg2 }
set -?[0-9]*\.?[0-9]+ float { type / const type arg2 int }    { ___DIV___ self arg2 }

comparison operations for floats
set -?[0-9]*\.?[0-9]+ float { type == const type arg2 int }   { ___EQ___  self arg2 }
set -?[0-9]*\.?[0-9]+ float { type == const type arg2 float } { ___EQ___  self arg2 }
set -?[0-9]*\.?[0-9]+ float { type != const type arg2 int }   { ___NEQ___ self arg2 }
set -?[0-9]*\.?[0-9]+ float { type != const type arg2 float } { ___NEQ___ self arg2 }
set -?[0-9]*\.?[0-9]+ float { type < const type arg2 int }    { ___LS___  self arg2 }
set -?[0-9]*\.?[0-9]+ float { type < const type arg2 float }  { ___LS___  self arg2 }
set -?[0-9]*\.?[0-9]+ float { type <= const type arg2 int }   { ___LQ___  self arg2 }
set -?[0-9]*\.?[0-9]+ float { type <= const type arg2 float } { ___LQ___  self arg2 }
set -?[0-9]*\.?[0-9]+ float { type > const type arg2 int }    { ___GT___  self arg2 }
set -?[0-9]*\.?[0-9]+ float { type > const type arg2 float }  { ___GT___  self arg2 }
set -?[0-9]*\.?[0-9]+ float { type >= const type arg2 int }   { ___GQ___  self arg2 }
set -?[0-9]*\.?[0-9]+ float { type >= const type arg2 float } { ___GQ___  self arg2 }

arithmetic operations for integers
set -?[0-9]+ int { type + const  type arg2 int }   { ___ADD___  self arg2 }
set -?[0-9]+ int { type + const  type arg2 float } { ___ADD___  self arg2 }
set -?[0-9]+ int { type - const  type arg2 int }   { ___SUB___  self arg2 }
set -?[0-9]+ int { type - const  type arg2 float } { ___SUB___  self arg2 }
set -?[0-9]+ int { type * const  type arg2 int }   { ___MUL___  self arg2 }
set -?[0-9]+ int { type * const  type arg2 float } { ___MUL___  self arg2 }
set -?[0-9]+ int { type / const  type arg2 int }   { ___DIV___  self arg2 }
set -?[0-9]+ int { type / const  type arg2 float } { ___DIV___  self arg2 }
set -?[0-9]+ int { type // const type arg2 int }   { ___IDIV___ self arg2 }
set -?[0-9]+ int { type % const  type arg2 int }   { ___MOD___  self arg2 }
set inc func { type a int } { a + 1 }
set dec func { type a int } { a - 1 }

comparison operations for integers
set -?[0-9]+ int { type == const type arg2 int }   { ___EQ___  self arg2 }
set -?[0-9]+ int { type == const type arg2 float } { ___EQ___  self arg2 }
set -?[0-9]+ int { type != const type arg2 int }   { ___NEQ___ self arg2 }
set -?[0-9]+ int { type != const type arg2 float } { ___NEQ___ self arg2 }
set -?[0-9]+ int { type < const type arg2 int }    { ___LS___  self arg2 }
set -?[0-9]+ int { type < const type arg2 float }  { ___LS___  self arg2 }
set -?[0-9]+ int { type <= const type arg2 int }   { ___LQ___  self arg2 }
set -?[0-9]+ int { type <= const type arg2 float } { ___LQ___  self arg2 }
set -?[0-9]+ int { type > const type arg2 int }    { ___GT___  self arg2 }
set -?[0-9]+ int { type > const type arg2 float }  { ___GT___  self arg2 }
set -?[0-9]+ int { type >= const type arg2 int }   { ___GQ___  self arg2 }
set -?[0-9]+ int { type >= const type arg2 float } { ___GQ___  self arg2 }

boolean operations
set True  bool { type && const type True const }  { type True bool }
set True  bool { type && const type False const } { type False bool }
set False bool { type && const type arg2 bool }   { type False bool }
set True  bool { type || const type arg2 bool }   { type True bool }
set False bool { type || const type True const }  { type True bool }
set False bool { type || const type False const } { type False bool }
set ! func { type True const }  { type False bool }
set ! func { type False const } { type True bool }

comparison operations for bools
set True bool  { type == const type True const }  { type True bool }
set True bool  { type == const type False const } { type False bool }
set False bool { type == const type True const }  { type False bool }
set False bool { type == const type False const } { type True bool }
set True bool  { type != const type True const }  { type False bool }
set True bool  { type != const type False const } { type True bool }
set False bool { type != const type True const }  { type True bool }
set False bool { type != const type False const } { type False bool }
set True bool  { type ^ type arg2 bool } { self != arg2 }
set False bool { type ^ type arg2 bool } { self != arg2 }

conditional functions
set if func { type True const  a            b }            { a }
set if func { type False const a            b }            { b }
set if func { type True const  a            type b int }   { a }
set if func { type False const a            type b int }   { b }
set if func { type True const  a            type b float } { a }
set if func { type False const a            type b float } { b }
set if func { type True const  a            type b bool }  { a }
set if func { type False const a            type b bool }  { b }
set if func { type True const  type a int   b }            { a }
set if func { type False const type a int   b }            { b }
set if func { type True const  type a int   type b int }   { a }
set if func { type False const type a int   type b int }   { b }
set if func { type True const  type a int   type b float } { a }
set if func { type False const type a int   type b float } { b }
set if func { type True const  type a int   type b bool }  { a }
set if func { type False const type a int   type b bool }  { b }
set if func { type True const  type a float b }            { a }
set if func { type False const type a float b }            { b }
set if func { type True const  type a float type b int }   { a }
set if func { type False const type a float type b int }   { b }
set if func { type True const  type a float type b float } { a }
set if func { type False const type a float type b float } { b }
set if func { type True const  type a float type b bool }  { a }
set if func { type False const type a float type b bool }  { b }
set if func { type True const  type a bool  b }            { a }
set if func { type False const type a bool  b }            { b }
set if func { type True const  type a bool  type b int }   { a }
set if func { type False const type a bool  type b int }   { b }
set if func { type True const  type a bool  type b float } { a }
set if func { type False const type a bool  type b float } { b }
set if func { type True const  type a bool  type b bool }  { a }
set if func { type False const type a bool  type b bool }  { b }

set repeat func { type n int x } { x type repeat func n - 1 x }
set repeat func { type 1 const x } { x }
set repeat func { type n int type x int } { x type repeat func n - 1 x }
set repeat func { type 1 const type x int } { x }
set repeat func { type n int type x float } { x type repeat func n - 1 x }
set repeat func { type 1 const type x float } { x }
set repeat func { type n int type x bool } { x type repeat func n - 1 x }
set repeat func { type 1 const type x bool } { x }

set ; ; { x } { ; }
set ; ; { type x int } { ; }
set ; ; { type x float } { ; }
set ; ; { type x bool } { ; }
