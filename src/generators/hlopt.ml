(*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)
open Hlcode

module ISet = Set.Make(struct
	let compare a b = b - a
	type t = int
end)

type cur_value =
	| VUndef
	| VReg of int

type reg_state = {
	mutable rindex : int;
	mutable ralias : reg_state;
	mutable rbind : reg_state list;
	mutable rnullcheck : bool;
}

type block = {
	bstart : int;
	mutable bend : int;
	mutable bnext : block list;
	mutable bprev : block list;
	mutable bloop : bool;
	mutable bstate : reg_state array option;
	mutable bneed : ISet.t;
	mutable bneed_all : ISet.t option;
	mutable bwrite : (int, int) PMap.t;
}

type control =
	| CNo
	| CJCond of int
	| CJAlways of int
	| CTry of int
	| CSwitch of int array
	| CRet
	| CThrow
	| CLabel

let control = function
	| OJTrue (_,d) | OJFalse (_,d) | OJNull (_,d) | OJNotNull (_,d)
	| OJSLt (_,_,d) | OJSGte (_,_,d) | OJSGt (_,_,d) | OJSLte (_,_,d) | OJULt (_,_,d) | OJUGte (_,_,d) | OJEq (_,_,d) | OJNotEq (_,_,d) | OJNotLt (_,_,d) | OJNotGte (_,_,d) ->
		CJCond d
	| OJAlways d ->
		CJAlways d
	| OLabel _ ->
		CLabel
	| ORet _ ->
		CRet
	| OThrow _ | ORethrow _ ->
		CThrow
	| OSwitch (_,cases,_) ->
		CSwitch cases
	| OTrap (_,d) ->
		CTry d
	| _ ->
		CNo

let opcode_fx frw op =
	let read r = frw r true and write r = frw r false in
	match op with
	| OMov (d,a) | ONeg (d,a) | ONot (d,a) ->
		read a; write d
	| OInt (d,_) | OFloat (d,_) | OBool (d,_) | OBytes (d,_) | OString (d,_) | ONull d ->
		write d
	| OAdd (d,a,b) | OSub (d,a,b) | OMul (d,a,b) | OSDiv (d,a,b) | OUDiv (d,a,b) | OSMod (d,a,b)| OUMod (d,a,b) | OShl (d,a,b) | OSShr (d,a,b) | OUShr (d,a,b) | OAnd (d,a,b) | OOr (d,a,b) | OXor (d,a,b) ->
		read a; read b; write d
	| OIncr a | ODecr a ->
		read a; write a
	| OCall0 (d,_) ->
		write d
	| OCall1 (d,_,a) ->
		read a; write d
	| OCall2 (d,_,a,b) ->
		read a; read b; write d
	| OCall3 (d,_,a,b,c) ->
		read a; read b; read c; write d
	| OCall4 (d,_,a,b,c,k) ->
		read a; read b; read c; read k; write d
	| OCallN (d,_,rl) | OCallMethod (d,_,rl) | OCallThis (d,_,rl) ->
		List.iter read rl; write d
	| OCallClosure (d,f,rl) ->
		read f; List.iter read rl; write d
	| OStaticClosure (d,_) ->
		write d
	| OInstanceClosure (d, _, a) | OVirtualClosure (d,a,_) ->
		read a; write d
	| OGetGlobal (d,_) ->
		write d
	| OSetGlobal (_,a) ->
		read a;
	| OField (d,a,_) | ODynGet (d,a,_) ->
		read a; write d
	| OSetField (a,_,b) | ODynSet (a,_,b)->
		read a; read b
	| OGetThis (d,_) ->
		write d
	| OSetThis (_,a) ->
		read a
	| OJTrue (r,_) | OJFalse (r,_) | OJNull (r,_) | OJNotNull (r,_) ->
		read r
	| OJSLt (a,b,_) | OJSGte (a,b,_) | OJSGt (a,b,_) | OJSLte (a,b,_) | OJULt (a,b,_) | OJUGte (a,b,_) | OJNotLt (a,b,_) | OJNotGte (a,b,_) | OJEq (a,b,_) | OJNotEq (a,b,_) ->
		read a; read b;
	| OJAlways _ | OLabel _ ->
		()
	| OToDyn (d, a) | OToSFloat (d,a) | OToUFloat (d,a) | OToInt (d,a) | OSafeCast (d,a) | OUnsafeCast (d,a) | OToVirtual (d,a) ->
		read a; write d
	| ORet r | OThrow r  | ORethrow r | OSwitch (r,_,_) | ONullCheck r ->
		read r
	| OTrap (r,_) ->
		write r
	| OEndTrap _ ->
		() (* ??? *)
	| OGetUI8 (d,a,b) | OGetUI16 (d,a,b) | OGetMem (d,a,b) | OGetArray (d,a,b) ->
		read a; read b; write d
	| OSetUI8 (a,b,c) | OSetUI16 (a,b,c) | OSetMem (a,b,c) | OSetArray (a,b,c) ->
		read a; read b; read c
	| ONew d ->
		write d
	| OArraySize (d, a)	| OGetType (d,a) | OGetTID (d,a) | OUnref (d,a) | OSetref (d, a) | OEnumIndex (d, a) | OEnumField (d,a,_,_) ->
		read a;
		write d
	| ORef (d, a) ->
		read a;
		write a; (* prevent issue with 'a' being reused later - this is not exact as it can be set everytime we pass it to a function *)
		write d;
	| OType (d,_) | OEnumAlloc (d,_) ->
		write d
	| OMakeEnum (d,_,rl) ->
		List.iter read rl;
		write d
	| OSetEnumField (a,_,b) ->
		read a; read b
	| OAssert _ ->
		()
	| ORefData (r,d) ->
		read d;
		write r;
	| ORefOffset (r,r2,off) ->
		read r2;
		read off;
		write r;
	| ONop _  ->
		()

let opcode_eq