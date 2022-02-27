(*
 *  This file is part of SwfLib
 *  Copyright (c)2004-2008 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *)
open Extlib_leftovers
open As3
open As3hl

type parse_ctx = {
	as3 : as3_tag;
	mutable namespaces : hl_namespace array;
	mutable nsets : hl_ns_set array;
	mutable names : hl_name array;
	mutable methods : hl_method array;
	mutable classes : hl_class array;
	mutable jumps : (int * int) list;
	mutable pos : int;
	delta_mt : int;
	delta_cl : int;
}

let get = As3parse.iget
let no_nz = As3parse.no_nz
let idx n = As3parse.index_int n - 1

let ident ctx i = get ctx.as3.as3_idents i
let name ctx n = ctx.names.(idx n)
let method_type ctx n = ctx.methods.(idx (no_nz n))
let getclass ctx n = ctx.classes.(idx (no_nz n))

let opt f ctx = function
	| None -> None
	| Some x -> Some (f ctx x)

let stack_delta = function
	| HBreakPoint -> 0
	| HNop -> 0
	| HThrow -> -1
	| HGetSuper _ -> 0
	| HSetSuper _ -> -2
	| HDxNs _ -> 0
	| HDxNsLate -> -1
	| HRegKill _ -> 0
	| HLabel -> 0
	| HJump (cond,_) ->
		(match cond with
		| J3Always -> 0
		| J3True
		| J3False -> -1
		| _ -> -2)
	| HSwitch _ -> -1
	| HPushWith -> -1
	| HPopScope -> 0
	| HForIn -> -1
	| HHasNext -> -1
	| HNull
	| HUndefined -> 1
	| HForEach -> -1
	| HSmallInt _
	| HInt _
	| HTrue
	| HFalse
	| HString _
	| HIntRef _
	| HUIntRef _
	| HFunction _
	| HFloat _
	| HNaN -> 1
	| HPop -> -1
	| HDup -> 1
	| HSwap -> 0
	| HScope -> -1
	| HNamespace _ -> 1
	| HNext _ -> 1
	| HCallStack n -> -(n + 1)
	| HConstruct n -> -n
	| HCallMethod (_,n) -> -n
	| HCallStatic (_,n) -> -n
	| HCallSuper (_,n) -> -n
	| HCallProperty (_,n) -> -n
	| HRetVoid -> 0
	| HRet -> -1
	| HConstructSuper n -> -(n + 1)
	| HConstructProperty (_,n) -> -n
	| HCallPropLex (_,n) -> -n
	| HCallSuperVoid (_,n) -> -(n + 1)
	| HCallPropVoid (_,n) -> -(n + 1)
	| HApplyType n -> -n
	| HObject n -> -(n * 2) + 1
	| HArray n -> -n + 1
	| HNewBlock -> 1
	| HClassDef _ -> 0
	| HGetDescendants _ -> 0
	| HCatch _ -> 1
	| HFindPropStrict _ -> 1
	| HFindProp _ -> 1
	| HFindDefinition _ -> 1
	| HGetLex _ -> 1
	| HSetProp _ -> -2
	| HReg _ -> 1
	| HSetReg _ | HSetThis -> -1
	| HGetGlobalScope | HGetScope _ -> 1
	| HGetProp _ -> 0
	| HInitProp _ -> -2
	| HDeleteProp _ -> -1 (* true/false *)
	| HGetSlot _ -> 0
	| HSetSlot _ -> -2
	| HToString
	| HToXml
	| HToXmlAttr
	| HToInt
	| HToUInt
	| HToNumber
	| HToObject
	| HAsAny
	| HAsType _
	| HIsType _
	| HAsObject
	| HAsString
	| HToBool -> 0
	| HCheckIsXml -> 0
	| HCast _ -> 0
	| HTypeof -> 0
	| HInstanceOf -> -1
	| HIncrReg _ | HDecrReg _ | HIncrIReg _ | HDecrIReg _ -> 0
	| HThis -> 1
	| HDebugReg _
	| HDebugLine _
	| HBreakPointLine _
	| HTimestamp
	| HDebugFile _ -> 0
	| HOp op ->
		(match op with
		| A3ONeg | A3OINeg | A3OIncr | A3ODecr | A3ONot | A3OBitNot | A3OIIncr | A3OIDecr -> 0
		| A3OMemGet8 | A3OMemGet16 | A3OMemGet32 | A3OMemGetFloat | A3OMemGetDouble | A3OSign1 | A3OSign8 | A3OSign16 -> 0
		| A3OMemSet8 | A3OMemSet16 | A3OMemSet32 | A3OMemSetFloat | A3OMemSetDouble -> -2
		| _ -> -1)
	| HUnk _ -> assert false

let parse_opcode ctx i = function
	| A3BreakPoint -> HBreakPoint
	| A3Nop -> HNop
	| A3Throw -> HThrow
	| A3GetSuper n -> HGetSup