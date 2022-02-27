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
	| A3GetSuper n -> HGetSuper (name ctx n)
	| A3SetSuper n -> HSetSuper (name ctx n)
	| A3DxNs s -> HDxNs (ident ctx s)
	| A3DxNsLate -> HDxNsLate
	| A3RegKill r -> HRegKill r
	| A3Label -> HLabel
	| A3Jump (j,n) ->
		ctx.jumps <- (i,ctx.pos) :: ctx.jumps;
		HJump (j,n)
	| A3Switch (n,infos) as op ->
		ctx.jumps <- (i,ctx.pos - As3code.length op) :: ctx.jumps;
		HSwitch(n,infos)
	| A3PushWith -> HPushWith
	| A3PopScope -> HPopScope
	| A3ForIn -> HForIn
	| A3HasNext -> HHasNext
	| A3Null -> HNull
	| A3Undefined -> HUndefined
	| A3ForEach -> HForEach
	| A3SmallInt n -> HSmallInt n
	| A3Int n -> HInt n
	| A3True -> HTrue
	| A3False -> HFalse
	| A3NaN -> HNaN
	| A3Pop -> HPop
	| A3Dup -> HDup
	| A3Swap -> HSwap
	| A3String i -> HString (ident ctx i)
	| A3IntRef i -> HIntRef (get ctx.as3.as3_ints i)
	| A3UIntRef i -> HUIntRef (get ctx.as3.as3_uints i)
	| A3Float f -> HFloat (get ctx.as3.as3_floats f)
	| A3Scope -> HScope
	| A3Namespace n -> HNamespace ctx.namespaces.(idx n)
	| A3Next (r1,r2) -> HNext (r1,r2)
	| A3Function f -> HFunction (method_type ctx f)
	| A3CallStack n -> HCallStack n
	| A3Construct n -> HConstruct n
	| A3CallMethod (s,n) -> HCallMethod (s,n)
	| A3CallStatic (m,n) -> HCallStatic (ctx.methods.(idx m),n)
	| A3CallSuper (p,n) -> HCallSuper (name ctx p,n)
	| A3CallProperty (p,n) -> HCallProperty (name ctx p,n)
	| A3RetVoid -> HRetVoid
	| A3Ret -> HRet
	| A3ConstructSuper n -> HConstructSuper n
	| A3ConstructProperty (p,n) -> HConstructProperty (name ctx p,n)
	| A3CallPropLex (p,n) -> HCallPropLex (name ctx p,n)
	| A3CallSuperVoid (p,n) -> HCallSuperVoid (name ctx p,n)
	| A3CallPropVoid (p,n) -> HCallPropVoid (name ctx p,n)
	| A3ApplyType n -> HApplyType n
	| A3Object n -> HObject n
	| A3Array n -> HArray n
	| A3NewBlock -> HNewBlock
	| A3ClassDef n -> HClassDef (getclass ctx n)
	| A3GetDescendants p -> HGetDescendants (name ctx p)
	| A3Catch n -> HCatch n
	| A3FindPropStrict p -> HFindPropStrict (name ctx p)
	| A3FindProp p -> HFindProp (name ctx p)
	| A3FindDefinition p -> HFindDefinition (name ctx p)
	| A3GetLex p -> HGetLex (name ctx p)
	| A3SetProp p -> HSetProp (name ctx p)
	| A3Reg r -> HReg r
	| A3SetReg r -> HSetReg r
	| A3GetGlobalScope -> HGetGlobalScope
	| A3GetScope n -> HGetScope n
	| A3GetProp p -> HGetProp (name ctx p)
	| A3InitProp p -> HInitProp (name ctx p)
	| A3DeleteProp p -> HDeleteProp (name ctx p)
	| A3GetSlot n -> HGetSlot n
	| A3SetSlot n -> HSetSlot n
	| A3ToString -> HToString
	| A3ToXml -> HToXml
	| A3ToXmlAttr -> HToXmlAttr
	| A3ToInt -> HToInt
	| A3ToUInt -> HToUInt
	| A3ToNumber -> HToNumber
	| A3ToBool -> HToBool
	| A3ToObject -> HToObject
	| A3CheckIsXml -> HCheckIsXml
	| A3Cast p -> HCast (name ctx p)
	| A3AsAny -> HAsAny
	| A3AsString -> HAsString
	| A3AsType p -> HAsType (name ctx p)
	| A3AsObject -> HAsObject
	| A3IncrReg r -> HIncrReg r
	| A3DecrReg r -> HDecrReg r
	| A3Typeof -> HTypeof
	| A3InstanceOf -> HInstanceOf
	| A3IsType p -> HIsType (name ctx p)
	| A3IncrIReg r -> HIncrIReg r
	| A3DecrIReg r -> HDecrIReg r
	| A3This -> HThis
	| A3SetThis -> HSetThis
	| A3DebugReg (id,r,n) -> HDebugReg (ident ctx id,r,n)
	| A3Deb