___SET___ setType setType ___"BLOCK___ pattern typeName ___BLOCK"___ ___"BLOCK___ set pattern typeName { } { } ___BLOCK"___
___SET___ =  =  ___"BLOCK___ ___BLOCK"___ ___"BLOCK___ ___BLOCK"___

set = ___SET___
type = ___TYPE___
self = ___SELF___
{ = ___"BLOCK___
} = ___BLOCK"___
\( = ___(BLOCK___
\) = ___BLOCK)___

set -?[0-9]*\.?[0-9]+ float { type op + type arg2 float } { ___ADD___ self arg2 }
set -?[0-9]*\.?[0-9]+ float { type op + type arg2 int }   { ___ADD___ self arg2 }
set -?[0-9]*\.?[0-9]+ float { type op - type arg2 float } { ___SUB___ self arg2 }
set -?[0-9]*\.?[0-9]+ float { type op - type arg2 int }   { ___SUB___ self arg2 }
set -?[0-9]*\.?[0-9]+ float { type op * type arg2 float } { ___MUL___ self arg2 }
set -?[0-9]*\.?[0-9]+ float { type op * type arg2 int }   { ___MUL___ self arg2 }
set -?[0-9]*\.?[0-9]+ float { type op / type arg2 float } { ___DIV___ self arg2 }
set -?[0-9]*\.?[0-9]+ float { type op / type arg2 int }   { ___DIV___ self arg2 }

set -?[0-9]+ int { type op +  type arg2 int }   { ___ADD___  self arg2 }
set -?[0-9]+ int { type op +  type arg2 float } { ___ADD___  self arg2 }
set -?[0-9]+ int { type op -  type arg2 int }   { ___SUB___  self arg2 }
set -?[0-9]+ int { type op -  type arg2 float } { ___SUB___  self arg2 }
set -?[0-9]+ int { type op *  type arg2 int }   { ___MUL___  self arg2 }
set -?[0-9]+ int { type op *  type arg2 float } { ___MUL___  self arg2 }
set -?[0-9]+ int { type op /  type arg2 int }   { ___DIV___  self arg2 }
set -?[0-9]+ int { type op /  type arg2 float } { ___DIV___  self arg2 }
set -?[0-9]+ int { type op // type arg2 int }   { ___IDIV___ self arg2 }
set -?[0-9]+ int { type op %  type arg2 int }   { ___MOD___  self arg2 }

set inc inc { type a int } { ___ADD___ a 1 }
set dec dec { type a int } { ___SUB___ a 1 }

set \+ +  { } { }
set -  -  { } { }
set \* *  { } { }
set /  /  { } { }
set // // { } { }
set %  %  { } { }
