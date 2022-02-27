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
	| A3DebugLine n -> HDebugLine n
	| A3DebugFile p -> HDebugFile (ident ctx p)
	| A3BreakPointLine n -> HBreakPointLine n
	| A3Timestamp -> HTimestamp
	| A3Op op -> HOp op
	| A3Unk n -> HUnk n

let parse_code ctx f trys =
	let code = f.fun3_code in
	let old = ctx.pos , ctx.jumps in
	let indexes = MultiArray.create() in
	ctx.pos <- 0;
	ctx.jumps <- [];
	let codepos pos delta =
		let id = (try MultiArray.get indexes (pos + delta) with _ -> -1) in
		if id = -1 then begin
			(*Printf.eprintf "MISALIGNED JUMP AT %d %c %d IN #%d\n" pos (if delta < 0 then '-' else '+') (if delta < 0 then -delta else delta) (idx (no_nz f.fun3_id));*)
			MultiArray.get indexes pos; (* jump 0 *)
		end else
			id
	in
	let hcode = MultiArray.mapi (fun i op ->
		let len = As3code.length op in
		MultiArray.add indexes i;
		for k = 2 to len do MultiArray.add indexes (-1); done;
		ctx.pos <- ctx.pos + len;
		parse_opcode ctx i op
	) code in
	(* in case we have a dead-jump at the end of code *)
	MultiArray.add indexes (MultiArray.length code);
	(* patch jumps *)
	List.iter (fun (j,pos) ->
		MultiArray.set hcode j (match MultiArray.get hcode j with
			| HJump (jc,n) ->
				HJump (jc,codepos pos n - j)
			| HSwitch (n,infos) ->
				HSwitch (codepos pos n - j, List.map (fun n -> codepos pos n - j) infos)
			| _ -> assert false)
	) ctx.jumps;
	(* patch try/catches *)
	Array.iteri (fun i t ->
		Array.set trys i {
			hltc_start = codepos 0 t.hltc_start;
			hltc_end = codepos 0 t.hltc_end;
			hltc_handle = codepos 0 t.hltc_handle;
			hltc_type = t.hltc_type;
			hltc_name = t.hltc_name;
		}
	) trys;
	ctx.pos <- fst old;
	ctx.jumps <- snd old;
	hcode

let parse_metadata ctx m =
	{
		hlmeta_name = ident ctx m.meta3_name;
		hlmeta_data = Array.map (fun (i1,i2) -> opt ident ctx i1, ident ctx i2) m.meta3_data;
	}

let parse_method ctx m =
	{
		hlm_type = method_type ctx m.m3_type;
		hlm_final = m.m3_final;
		hlm_override = m.m3_override;
		hlm_kind = m.m3_kind;
	}

let parse_value ctx = function
	| A3VNone -> HVNone
	| A3VNull -> HVNull
	| A3VBool b -> HVBool b
	| A3VString s -> HVString (ident ctx s)
	| A3VInt i -> HVInt (get ctx.as3.as3_ints i)
	| A3VUInt i -> HVUInt (get ctx.as3.as3_uints i)
	| A3VFloat f -> HVFloat (get ctx.as3.as3_floats f)
	| A3VNamespace (n,ns) -> HVNamespace (n,ctx.namespaces.(idx ns))

let parse_var ctx v =
	{
		hlv_type = opt name ctx v.v3_type;
		hlv_value = parse_value ctx v.v3_value;
		hlv_const = v.v3_const;
	}

let parse_field_kind ctx = function
	| A3FMethod m -> HFMethod (parse_method ctx m)
	| A3FVar v -> HFVar (parse_var ctx v)
	| A3FFunction f -> HFFunction (method_type ctx f)
	| A3FClass c -> HFClass (getclass ctx c)

let parse_field ctx f =
	{
		hlf_name = name ctx f.f3_name;
		hlf_slot = f.f3_slot;
		hlf_kind = parse_field_kind ctx f.f3_kind;
		hlf_metas =
			match f.f3_metas with
			| None -> None
			| Some a ->
				Some (Array.map (fun i ->
					parse_metadata ctx (get ctx.as3.as3_metadatas (no_nz i))
				) a);
	}

let parse_static ctx s =
	{
		hls_method = method_type ctx s.st3_method;
		hls_fields = Array.map (parse_field ctx) s.st3_fields;
	}

let parse_namespace ctx = function
	|