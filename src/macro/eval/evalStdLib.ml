
(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)
open Extlib_leftovers
open Globals
open EvalValue
open EvalEncode
open EvalDecode
open EvalContext
open EvalExceptions
open EvalPrinting
open EvalMisc
open EvalField
open EvalHash
open EvalString
open EvalThread

let catch_unix_error f arg =
	try
		f arg
	with Unix.Unix_error(err,cmd,args) ->
		exc_string (Printf.sprintf "%s(%s, %s)" (Unix.error_message err) cmd args)

let ptmap_keys h =
	IntMap.fold (fun k _ acc -> k :: acc) h []

let hashtbl_keys h =
	Hashtbl.fold (fun k _ acc -> k :: acc) h []

module StdEvalVector = struct
	let this this = match this with
		| VVector vv -> vv
		| v -> unexpected_value v "vector"

	let blit = vifun4 (fun vthis srcPos dest destPos len ->
		Array.blit (this vthis) (decode_int srcPos) (decode_vector dest) (decode_int destPos) (decode_int len);
		vnull
	)

	let toArray = vifun0 (fun vthis ->
		let copy = Array.copy (this vthis) in
		encode_array_instance (EvalArray.create copy)
	)

	let fromArrayCopy = vfun1 (fun arr ->
		let a = decode_varray arr in
		encode_vector_instance (Array.sub a.avalues 0 a.alength)
	)

	let copy = vifun0 (fun vthis ->
		encode_vector_instance (Array.copy (this vthis))
	)

	let join = vifun1 (fun vthis sep ->
		let this = this vthis in
		let sep = decode_vstring sep in
		vstring ((EvalArray.array_join this (s_value 0) sep))
	)

	let map = vifun1 (fun vthis f ->
		let this = this vthis in
		let a = match f with
			| VFunction(f,_) ->
				Array.map (fun v -> f [v]) this
			| VFieldClosure(v1,f) ->
				Array.map (fun v -> f (v1 :: [v])) this
			| _ -> exc_string ("Cannot call " ^ (value_string f))
		in
		encode_vector_instance a
	)
end

module StdArray = struct
	let this this = match this with
		| VArray va -> va
		| v -> unexpected_value v "array"

	let concat = vifun1 (fun vthis a2 ->
		let a2 = decode_varray a2 in
		encode_array_instance (EvalArray.concat (this vthis) a2)
	)

	let copy = vifun0 (fun vthis ->
		encode_array_instance (EvalArray.copy (this vthis))
	)

	let filter = vifun1 (fun vthis f ->
		let this = this vthis in
		let a = EvalArray.filter this (fun v -> is_true (call_value_on vthis f [v])) in
		encode_array_instance a
	)

	let indexOf = vifun2 (fun vthis x fromIndex ->
		let this = this vthis in
		let fromIndex = default_int fromIndex 0 in
		let fromIndex = if fromIndex < 0 then this.alength + fromIndex else fromIndex in
		let fromIndex = if fromIndex < 0 then 0 else fromIndex in
		vint (EvalArray.indexOf this equals x fromIndex)
	)

	let insert = vifun2 (fun vthis pos x ->
		let this = this vthis in
		let pos = decode_int pos in
		if pos >= this.alength then begin
			ignore(EvalArray.push this x);
		end else begin
			let pos = if pos < 0 then this.alength + pos else pos in
			let pos = if pos < 0 then 0 else pos in
			EvalArray.insert this pos x
		end;
		vnull
	)

	let iterator = vifun0 (fun vthis ->
		let this = this vthis in
		let f_has_next,f_next = EvalArray.iterator this in
		encode_obj [
			key_hasNext,vifun0 (fun _ -> vbool (f_has_next()));
			key_next,vifun0 (fun _ -> f_next())
		]
	)

	let join = vifun1 (fun vthis sep ->
		let sep = decode_vstring sep in
		let s = EvalArray.join (this vthis) (s_value 0) sep in
		vstring s
	)

	let keyValueIterator = vifun0 (fun vthis ->
		let ctx = get_ctx() in
		let path = key_haxe_iterators_array_key_value_iterator in
		let vit = encode_instance path in
		let fnew = get_instance_constructor ctx path null_pos in
		ignore(call_value_on vit (Lazy.force fnew) [vthis]);
		vit
	)

	let lastIndexOf = vifun2 (fun vthis x fromIndex ->
		let this = this vthis in
		let last = this.alength - 1 in
		let fromIndex = default_int fromIndex last in
		let fromIndex = if fromIndex < 0 then this.alength + fromIndex else fromIndex in
		let fromIndex = if fromIndex < 0 then 0 else if fromIndex > last then last else fromIndex in
		vint (EvalArray.lastIndexOf this equals x fromIndex)
	)

	let map = vifun1 (fun vthis f ->
		let this = this vthis in
		let a = match f with
			| VFunction(f,_) ->
				EvalArray.map this (fun v -> f [v])
			| VFieldClosure(v1,f) ->
				EvalArray.map this (fun v -> f (v1 :: [v]))
			| _ -> exc_string ("Cannot call " ^ (value_string f))
		in
		encode_array_instance a
	)

	let pop = vifun0 (fun vthis ->
		let this = this vthis in
		EvalArray.pop this
	)

	let push = vifun1 (fun vthis v ->
		let this = this vthis in
		vint32 (Int32.of_int (EvalArray.push this v))
	)

	let remove = vifun1 (fun vthis x ->
		let this = this vthis in
		vbool (EvalArray.remove this equals x)
	)

	let contains = vifun1 (fun vthis x ->
		let this = this vthis in
		vbool (EvalArray.contains this equals x)
	)

	let reverse = vifun0 (fun vthis ->
		let this = this vthis in
		EvalArray.reverse this;
		vnull
	)

	let shift = vifun0 (fun vthis ->
		let this = this vthis in
		EvalArray.shift this
	)

	let slice = vifun2 (fun vthis pos end' ->
		let this = this vthis in
		let pos = decode_int pos in
		let length = this.alength in
		let end' = default_int end' length in
		let end' = if end' > length then length else end' in
		let pos = if pos < 0 then length + pos else pos in
		let end' = if end' < 0 then length + end' else end' in
		let pos = if pos < 0 then 0 else pos in
		let end' = if end' < 0 then 0 else end' in
		encode_array_instance (EvalArray.slice this pos end')
	)

	let sort = vifun1 (fun vthis f ->
		let this = this vthis in
		EvalArray.sort this (fun a b -> decode_int (call_value_on vthis f [a;b]));
		vnull
	)

	let splice = vifun2 (fun vthis pos len ->
		let this = this vthis in
		let pos = decode_int pos in
		let len = decode_int len in
		let length = this.alength in
		if len < 0 || pos > length then
			encode_array []
		else begin
			let pos = if pos < 0 then length + pos else pos in
			let pos = if pos < 0 then 0 else pos in
			let delta = length - pos in
			let len = if len > delta then delta else len in
			let end' = pos + len in
			encode_array_instance (EvalArray.splice this pos len end')
		end
	)

	let toString = vifun0 (fun vthis ->
		vstring (s_array 0 0 (this vthis))
	)

	let unshift = vifun1 (fun vthis v ->
		let this = this vthis in
		EvalArray.unshift this v;
		vnull
	)

	let resize = vifun1 (fun vthis len ->
		let this = this vthis in
		let len = decode_int len in
		EvalArray.resize this len;
		vnull
	)
end

let outside_bounds () =
	let haxe_io_Error = get_static_prototype (get_ctx()) key_haxe_io_Error null_pos in
	exc (proto_field_direct haxe_io_Error key_OutsideBounds)

module StdBytes = struct
	open EvalBytes

	let this vthis = match vthis with
		| VInstance {ikind = IBytes o} -> o
		| v -> unexpected_value v "bytes"

	let alloc = vfun1 (fun length ->
		let length = decode_int length in
		encode_bytes (Bytes.make length (Char.chr 0))
	)

	let encode_native v = match v with
		| VEnumValue {eindex = 1} -> true (* haxe.io.Encoding.RawNative *)
		| _ -> false

	let blit = vifun4 (fun vthis pos src srcpos len ->
		let s = this vthis in
		let pos = decode_int pos in
		let src = decode_bytes src in
		let srcpos = decode_int srcpos in
		let len = decode_int len in
		(try Bytes.blit src srcpos s pos len with _ -> outside_bounds());
		vnull
	)

	let compare = vifun1 (fun vthis other ->
		let this = this vthis in
		let other = decode_bytes other in
		vint (Pervasives.compare this other)
	)

	let fastGet = vfun2 (fun b pos ->
		let b = decode_bytes b in
		let pos = decode_int pos in
		try vint (int_of_char (Bytes.unsafe_get b pos)) with _ -> vnull
	)

	let fill = vifun3 (fun vthis pos len value ->
		let this = this vthis in
		let pos = decode_int pos in
		let len = decode_int len in
		let value = decode_int value in
		(try Bytes.fill this pos len (char_of_int (value land 0xFF)) with _ -> outside_bounds());
		vnull
	)

	let get = vifun1 (fun vthis pos ->
		let this = this vthis in
		let pos = decode_int pos in
		try vint (read_byte this pos) with _ -> vnull
	)

	let getData = vifun0 (fun vthis -> vthis)

	let getDouble = vifun1 (fun vthis pos ->
		try vfloat (Int64.float_of_bits (read_i64 (this vthis) (decode_int pos))) with _ -> outside_bounds()
	)

	let getFloat = vifun1 (fun vthis pos ->
		try vfloat (Int32.float_of_bits (read_i32 (this vthis) (decode_int pos))) with _ -> outside_bounds()
	)

	let getInt32 = vifun1 (fun vthis pos ->
		try vint32 (read_i32 (this vthis) (decode_int pos)) with exc -> outside_bounds()
	)

	let getInt64 = vifun1 (fun vthis pos ->
		let this = this vthis in
		let pos = decode_int pos in
		try
			let low = read_i32 this pos in
			let high = read_i32 this (pos + 4) in
			EvalIntegers.encode_haxe_i64 low high;
		with _ ->
			outside_bounds()
	)

	let getString = vifun3 (fun vthis pos len encoding ->
		let this = this vthis in
		let pos = decode_int pos in
		let len = decode_int len in
		let s = try Bytes.sub this pos len with _ -> outside_bounds() in
		create_unknown (Bytes.unsafe_to_string s)
	)

	let getUInt16 = vifun1 (fun vthis pos ->
		try vint (read_ui16 (this vthis) (decode_int pos)) with _ -> outside_bounds()
	)

	let ofData = vfun1 (fun v -> v)

	let ofString = vfun2 (fun v encoding ->
		let s = decode_vstring v in
		encode_bytes (Bytes.of_string s.sstring)
	)

	let ofHex = vfun1 (fun v ->
		let s = decode_string v in
		let len = String.length s in
		if (len land 1) <> 0 then exc_string "Not a hex string (odd number of digits)";
		let ret = (Bytes.make (len lsr 1) (Char.chr 0)) in
		for i = 0 to Bytes.length ret - 1 do
			let high = int_of_char s.[i * 2] in
			let low = int_of_char s.[i * 2 + 1] in
			let high = (high land 0xF) + ((high land 0x40) lsr 6) * 9 in
			let low = (low land 0xF) + ((low land 0x40) lsr 6) * 9 in
			Bytes.set ret i (char_of_int (((high lsl 4) lor low) land 0xFF));
		done;
		encode_bytes ret
	)

	let set = vifun2 (fun vthis pos v ->
		let this = this vthis in
		let pos = decode_int pos in
		let v = decode_int v in
		(try write_byte this pos v with _ -> ());
		vnull;
	)

	let setDouble = vifun2 (fun vthis pos v ->
		(try write_i64 (this vthis) (decode_int pos) (Int64.bits_of_float (num v)) with _ -> outside_bounds());
		vnull
	)

	let setFloat = vifun2 (fun vthis pos v ->
		let this = this vthis in
		let pos = decode_int pos in
		let v = num v in
		write_i32 this pos (Int32.bits_of_float v);
		vnull
	)

	let setInt32 = vifun2 (fun vthis pos v ->
		(try write_i32 (this vthis) (decode_int pos) (decode_i32 v) with _ -> outside_bounds());
		vnull;
	)

	let setInt64 = vifun2 (fun vthis pos v ->
		let v = decode_instance v in
		let pos = decode_int pos in
		let high = decode_i32 (instance_field v key_high) in
		let low = decode_i32 (instance_field v key_low) in
		let this = this vthis in
		try
			write_i32 this pos low;
			write_i32 this (pos + 4) high;
			vnull
		with _ ->
			outside_bounds()
	)

	let setUInt16 = vifun2 (fun vthis pos v ->
		(try write_ui16 (this vthis) (decode_int pos) (decode_int v land 0xFFFF) with _ -> outside_bounds());
		vnull
	)

	let sub = vifun2 (fun vthis pos len ->
		let this = this vthis in
		let pos = decode_int pos in
		let len = decode_int len in
		let s = try Bytes.sub this pos len with _ -> outside_bounds() in
		encode_bytes s
	)

	let toHex = vifun0 (fun vthis ->
		let this = this vthis in
		let chars = [|"0";"1";"2";"3";"4";"5";"6";"7";"8";"9";"a";"b";"c";"d";"e";"f"|] in
		let l = Bytes.length this in
		let rec loop acc i =
			if i >= l then List.rev acc
			else begin
				let c = int_of_char (Bytes.get this i) in
				loop ((chars.(c land 15)) :: ((chars.(c lsr 4))) :: acc) (i + 1)
			end
		in
		encode_string (String.concat "" (loop [] 0))
	)

	let toString = vifun0 (fun vthis ->
		let this = this vthis in
		try
			UTF8.validate (Bytes.unsafe_to_string this);
			(create_unknown (Bytes.to_string this))
		with UTF8.Malformed_code ->
			exc_string "Invalid string"
	)
end

module StdBytesBuffer = struct
	let this vthis = match vthis with
		| VInstance {ikind = IOutput o} -> o
		| v -> unexpected_value v "output"

	let get_length = vifun0 (fun vthis ->
		let this = this vthis in
		vint (Buffer.length this)
	)

	let add_char this i =
		Buffer.add_char this (Char.unsafe_chr i)

	let add_i32 this v =
		let base = Int32.to_int v in
		let big = Int32.to_int (Int32.shift_right_logical v 24) in
		add_char this base;
		add_char this (base lsr 8);
		add_char this (base lsr 16);
		add_char this big

	let addByte = vifun1 (fun vthis byte ->
		let this = this vthis in
		let byte = decode_int byte in
		add_char this byte;
		vnull;
	)

	let add = vifun1 (fun vthis src ->
		let this = this vthis in
		let src = decode_bytes src in
		Buffer.add_bytes this src;
		vnull
	)

	let addString = vifun2 (fun vthis src encoding ->
		let this = this vthis in
		let src = decode_vstring src in
		Buffer.add_string this src.sstring;
		vnull
	)

	let addInt32 = vifun1 (fun vthis v ->
		let this = this vthis in
		let v = decode_i32 v in
		add_i32 this v;
		vnull
	)

	let addInt64 = vifun1 (fun vthis v ->
		let this = this vthis in
		let v = decode_instance v in
		let high = decode_i32 (instance_field v key_high) in
		let low = decode_i32 (instance_field v key_low) in
		add_i32 this low;
		add_i32 this high;
		vnull;
	)

	let addFloat = vifun1 (fun vthis v ->
		let this = this vthis in
		let v = num v in
		add_i32 this (Int32.bits_of_float v);
		vnull
	)

	let addDouble = vifun1 (fun vthis v ->
		let this = this vthis in
		let v = num v in
		let v = Int64.bits_of_float v in
		add_i32 this (Int64.to_int32 v);
		add_i32 this (Int64.to_int32 (Int64.shift_right_logical v 32));
		vnull
	)

	let addBytes = vifun3 (fun vthis src pos len ->
		let this = this vthis in
		let src = decode_bytes src in
		let pos = decode_int pos in
		let len = decode_int len in
		if pos < 0 || len < 0 || pos + len > Bytes.length src then outside_bounds();
		Buffer.add_subbytes this src pos len;
		vnull
	)

	let getBytes = vifun0 (fun vthis ->
		let this = this vthis in
		encode_bytes (Bytes.unsafe_of_string (Buffer.contents this))
	)
end

module StdCompress = struct
	open Extc

	type zfun = zstream -> src:string -> spos:int -> slen:int -> dst:bytes -> dpos:int -> dlen:int -> zflush -> zresult

	let this vthis = match vthis with
		| VInstance {ikind = IZip zip} -> zip
		| _ -> unexpected_value vthis "Compress"

	let exec (f : zfun) vthis src srcPos dst dstPos =
		let this = this vthis in
		let src = decode_bytes src in
		let srcPos = decode_int srcPos in
		let dst = decode_bytes dst in
		let dstPos = decode_int dstPos in
		let r = try f this.z (Bytes.unsafe_to_string src) srcPos (Bytes.length src - srcPos) dst dstPos (Bytes.length dst - dstPos) this.z_flush with _ -> exc_string "oops" in
		encode_obj [
			key_done,vbool r.z_finish;
			key_read,vint r.z_read;
			key_write,vint r.z_wrote
		]

	let close = vifun0 (fun vthis ->
		zlib_deflate_end (this vthis).z;
		vnull
	)

	let execute = vifun4 (fun vthis src srcPos dst dstPos ->
		exec zlib_deflate vthis src srcPos dst dstPos
	)

	let run = vfun2 (fun s level ->
		let s = decode_bytes s in
		let level = decode_int level in
		let zip = zlib_deflate_init level in
		let d = Bytes.make (zlib_deflate_bound zip (Bytes.length s)) (char_of_int 0) in
		let r = zlib_deflate zip (Bytes.unsafe_to_string s) 0 (Bytes.length s) d 0 (Bytes.length d) Z_FINISH in
		zlib_deflate_end zip;
		if not r.z_finish || r.z_read <> (Bytes.length s) then exc_string "Compression failed";
		encode_bytes (Bytes.sub d 0 r.z_wrote)
	)

	let setFlushMode = vifun1 (fun vthis f ->
		let mode = match fst (decode_enum f) with
			| 0 -> Z_NO_FLUSH
			| 1 -> Z_SYNC_FLUSH
			| 2 -> Z_FULL_FLUSH
			| 3 -> Z_FINISH
			| 4 -> Z_PARTIAL_FLUSH
			| _ -> die "" __LOC__
		in
		(this vthis).z_flush <- mode;
		vnull
	)
end

module StdContext = struct
	let addBreakpoint = vfun2 (fun file line ->
		let file = decode_string file in
		let line = decode_int line in
		begin try
			ignore(EvalDebugMisc.add_breakpoint (get_ctx()) file line BPAny None);
		with Not_found ->
			exc_string ("Could not find file " ^ file)
		end;
		vnull
	)

	let breakHere = vfun0 (fun () ->
		if not ((get_ctx()).debug.support_debugger) then vnull
		else raise (EvalDebugMisc.BreakHere)
	)

	let callMacroApi = vfun1 (fun f ->
		let f = decode_string f in
		Hashtbl.find GlobalState.macro_lib f
	)

	let plugins = ref PMap.empty

	let plugin_data = ref None

	let register data = plugin_data := Some data

	let loadPlugin = vfun1 (fun filePath ->
		let filePath = decode_string filePath in
		let filePath = Dynlink.adapt_filename filePath in
		if PMap.mem filePath !plugins then
			PMap.find filePath !plugins
		else begin
			(try Dynlink.loadfile filePath with Dynlink.Error error -> exc_string (Dynlink.error_message error));
			match !plugin_data with
				| Some l ->
					let vapi = encode_obj_s l in
					plugins := PMap.add filePath vapi !plugins;
					vapi
				| None ->
					vnull
		end
	)
end

module StdCrc32 = struct
	let make = vfun1 (fun data ->
		let data = decode_bytes data in
		let crc32 = Extc.zlib_crc32 data (Bytes.length data) in
		vint32 crc32
	)
end

module StdDate = struct
	open Unix

	let encode_date d = encode_instance key_Date ~kind:(IDate d)

	let this vthis = match vthis with
		| VInstance {ikind = IDate d} -> d
		| v -> unexpected_value v "date"

	let fromTime = vfun1 (fun f -> encode_date ((num f) /. 1000.))

	let fromString = vfun1 (fun s ->
		let s = decode_string s in
		match String.length s with
		| 19 ->
			let r = Str.regexp "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\) \\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)$" in
			if not (Str.string_match r s 0) then exc_string ("Invalid date format : " ^ s);
			let t = {
				tm_year = int_of_string (Str.matched_group 1 s) - 1900;
				tm_mon = int_of_string (Str.matched_group 2 s) - 1;
				tm_mday = int_of_string (Str.matched_group 3 s);
				tm_hour = int_of_string (Str.matched_group 4 s);
				tm_min = int_of_string (Str.matched_group 5 s);
				tm_sec = int_of_string (Str.matched_group 6 s);
				tm_wday = 0;
				tm_yday = 0;
				tm_isdst = false;
			} in
			encode_date (fst (catch_unix_error mktime t))
		| 10 ->
			let r = Str.regexp "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)$" in
			if not (Str.string_match r s 0) then exc_string ("Invalid date format : " ^ s);
			let t = {
				tm_year = int_of_string (Str.matched_group 1 s) - 1900;
				tm_mon = int_of_string (Str.matched_group 2 s) - 1;
				tm_mday = int_of_string (Str.matched_group 3 s);
				tm_hour = 0;
				tm_min = 0;
				tm_sec = 0;
				tm_wday = 0;
				tm_yday = 0;
				tm_isdst = false;
			} in
			encode_date (fst (catch_unix_error mktime t))
		| 8 ->
			let r = Str.regexp "^\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)$" in
			if not (Str.string_match r s 0) then exc_string ("Invalid date format : " ^ s);
			let h = int_of_string (Str.matched_group 1 s) in
			let m = int_of_string (Str.matched_group 2 s) in
			let s = int_of_string (Str.matched_group 3 s) in
			let t = h * 60 * 60 + m * 60 + s in
			encode_date (float_of_int t)
		| _ ->
			exc_string ("Invalid date format : " ^ s)
	)

	let getDate = vifun0 (fun vthis -> vint (catch_unix_error localtime (this vthis)).tm_mday)
	let getDay = vifun0 (fun vthis -> vint (catch_unix_error localtime (this vthis)).tm_wday)
	let getFullYear = vifun0 (fun vthis -> vint (((catch_unix_error localtime (this vthis)).tm_year) + 1900))
	let getHours = vifun0 (fun vthis -> vint (catch_unix_error localtime (this vthis)).tm_hour)
	let getMinutes = vifun0 (fun vthis -> vint (catch_unix_error localtime (this vthis)).tm_min)
	let getMonth = vifun0 (fun vthis -> vint (catch_unix_error localtime (this vthis)).tm_mon)
	let getSeconds = vifun0 (fun vthis -> vint (catch_unix_error localtime (this vthis)).tm_sec)
	let getUTCDate = vifun0 (fun vthis -> vint (catch_unix_error gmtime (this vthis)).tm_mday)
	let getUTCDay = vifun0 (fun vthis -> vint (catch_unix_error gmtime (this vthis)).tm_wday)
	let getUTCFullYear = vifun0 (fun vthis -> vint (((catch_unix_error gmtime (this vthis)).tm_year) + 1900))
	let getUTCHours = vifun0 (fun vthis -> vint (catch_unix_error gmtime (this vthis)).tm_hour)
	let getUTCMinutes = vifun0 (fun vthis -> vint (catch_unix_error gmtime (this vthis)).tm_min)
	let getUTCMonth = vifun0 (fun vthis -> vint (catch_unix_error gmtime (this vthis)).tm_mon)
	let getUTCSeconds = vifun0 (fun vthis -> vint (catch_unix_error gmtime (this vthis)).tm_sec)
	let getTime = vifun0 (fun vthis -> vfloat ((this vthis) *. 1000.))
	let getTimezoneOffset = vifun0 (fun vthis ->
		let tmLocal = catch_unix_error localtime (this vthis) in
		let tmUTC = catch_unix_error gmtime (this vthis) in
		let tsLocal = fst (catch_unix_error mktime tmLocal) in
		let tsUTC = fst (catch_unix_error mktime tmUTC) in
		vint (int_of_float ((tsUTC -. tsLocal) /. 60.))
	)
	let now = vfun0 (fun () -> encode_date (catch_unix_error time()))
	let toString = vifun0 (fun vthis -> vstring (s_date (this vthis)))
end

module StdDeque = struct
	let this vthis = match vthis with
		| VInstance {ikind = IDeque d} -> d
		| _ -> unexpected_value vthis "Deque"

	let add = vifun1 (fun vthis i ->
		let this = this vthis in
		Deque.add this i;
		vnull
	)

	let pop = vifun1 (fun vthis blocking ->
		let this = this vthis in
		let blocking = decode_bool blocking in
		match Deque.pop this blocking with
		| None -> vnull
		| Some v -> v
	)

	let push = vifun1 (fun vthis i ->
		let this = this vthis in
		Deque.push this i;
		vnull
	)
end

module StdEReg = struct
	open Pcre

	let create r opt =
		let open Pcre in
		let string_of_pcre_error = function
			| BadPattern(s,i) -> Printf.sprintf "at %i: %s" i s
			| Partial -> "Partial"
			| BadPartial -> "BadPartial"
			| BadUTF8 -> "BadUTF8"
			| BadUTF8Offset -> "BadUTF8Offset"
			| MatchLimit -> "MatchLimit"
			| RecursionLimit -> "RecursionLimit"
			| InternalError s -> "InternalError: " ^ s
		in
		let global = ref false in
		let flags = ExtList.List.filter_map (function
			| 'i' -> Some `CASELESS
			| 's' -> Some `DOTALL
			| 'm' -> Some `MULTILINE
			| 'u' -> None
			| 'g' -> global := true; None
			| c -> failwith ("Unsupported regexp option '" ^ String.make 1 c ^ "'")
		) (ExtString.String.explode opt) in
		let flags = `UTF8 :: `UCP :: flags in
		let rex = try regexp ~flags r with Error error -> failwith (string_of_pcre_error error) in
		let pcre = {
			r = rex;
			r_rex_string = create_ascii (Printf.sprintf "~/%s/%s" r opt);
			r_global = !global;
			r_string = "";
			r_groups = [||]
		} in
		IRegex pcre

	let maybe_run rex n f =
		let substrings = if Array.length rex.r_groups = 0 then exc_string "Invalid regex operation because no match was made" else rex.r_groups.(0) in
		if n < 0 || n >= num_of_subs substrings then exc_string "Invalid group"
		else try f (get_substring_ofs substrings n)
		with Not_found -> vnull

	let this this = match this with
		| VInstance {ikind = IRegex rex} -> rex
		| v -> unexpected_value v "EReg"

	let escape = vfun1 (fun s ->
		let s = decode_string s in
		create_unknown (Str.quote s)
	)

	let map = vifun2 (fun vthis s f ->
		let this = this vthis in
		let s = decode_string s in
		let l = String.length s in
		let buf = Buffer.create 0 in
		let rec loop pos =
			if pos >= l then
				()
			else begin try
				let a = exec ~rex:this.r ~pos s in
				this.r_groups <- [|a|];
				let (first,last) = get_substring_ofs a 0 in
				Buffer.add_substring buf s pos (first - pos);
				Buffer.add_string buf (decode_string (call_value_on vthis f [vthis]));
				if last = first then begin
					if last >= l then
						()
					else begin
						if this.r_global then begin
							Buffer.add_substring buf s first 1;
							loop (first + 1)
						end else
							Buffer.add_substring buf s first (l - first)
					end
				end else if this.r_global then
					loop last
				else
					Buffer.add_substring buf s last (l - last)
			with Not_found ->
				Buffer.add_substring buf s pos (l - pos)
			end
		in
		this.r_string <- s;
		loop 0;
		this.r_string <- "";
		this.r_groups <- [||];
		create_unknown (Buffer.contents buf)
	)

	let match' = vifun1 (fun vthis s ->
		let this = this vthis in
		let open Pcre in
		let s = decode_string s in
		this.r_string <- s;
		try
			let a = exec_all ~iflags:0x2000 ~rex:this.r s in
			this.r_groups <- a;
			vtrue
		with Not_found ->
			this.r_groups <- [||];
			vfalse
		| Pcre.Error _ ->
			exc_string "PCRE Error (invalid unicode string?)"
	)

	let matched = vifun1 (fun vthis n ->
		let this = this vthis in
		let n = decode_int n in
		maybe_run this n (fun (first,last) ->
			create_unknown (ExtString.String.slice ~first ~last this.r_string)
		)
	)

	let matchedLeft = vifun0 (fun vthis ->
		let this = this vthis in
		maybe_run this 0 (fun (first,_) ->
			create_unknown (ExtString.String.slice ~last:first this.r_string)
		)
	)

	let matchedPos = vifun0 (fun vthis ->
		let this = this vthis in
		let rec byte_offset_to_char_offset_lol s i k o =
			if i = 0 then
				k
			else begin
				let n = UTF8.next s o in
				let d = n - o in
				byte_offset_to_char_offset_lol s (i - d) (k + 1) n
			end
		in
		maybe_run this 0 (fun (first,last) ->
			let first = byte_offset_to_char_offset_lol this.r_string first 0 0 in
			let last = byte_offset_to_char_offset_lol this.r_string last 0 0 in
			encode_obj [key_pos,vint first;key_len,vint (last - first)]
		)
	)

	let matchedRight = vifun0 (fun vthis ->
		let this = this vthis in
		maybe_run this 0 (fun (_,last) ->
			create_unknown (ExtString.String.slice ~first:last this.r_string)
		)
	)

	let matchSub = vifun3 (fun vthis s pos len ->
		let this = this vthis in
		let s = decode_string s in
		let pos = decode_int pos in
		let len_default = String.length s - pos in
		let len = default_int len len_default in
		let len = if len < 0 then len_default else len in
		begin try
			if pos + len > String.length s then raise Not_found;
			let str = String.sub s 0 (pos + len) in
			let a = Pcre.exec_all ~iflags:0x2000 ~rex:this.r ~pos str in
			this.r_string <- s;
			this.r_groups <- a;
			vtrue
		with Not_found ->
			vfalse
		end
	)

	let replace = vifun2 (fun vthis s by ->
		let this = this vthis in
		let s = decode_string s in
		let by = decode_string by in
		let s = (if this.r_global then Pcre.replace else Pcre.replace_first) ~iflags:0x2000 ~rex:this.r ~templ:by s in
		create_unknown s
	)

	let split = vifun1 (fun vthis s ->
		let this = this vthis in
		let s = decode_string s in
		let slength = String.length s in
		if slength = 0 then
			encode_array [v_empty_string]
		else begin
			let copy_offset = ref 0 in
			let acc = DynArray.create () in
			let add first last =
				let sub = String.sub s first (last - first) in
				DynArray.add acc (create_unknown sub)
			in
			let exec = Pcre.exec ~iflags:0x2000 ~rex:this.r in
			let step pos =
				try
					let substrings = exec ~pos s in
					let (first,last) = Pcre.get_substring_ofs substrings 0 in
					add !copy_offset first;
					copy_offset := last;
					let next_start = if pos = last then last + 1 else last in
					if next_start >= slength then begin
						DynArray.add acc (create_unknown "");
						None
					end else
						Some next_start
				with Not_found ->
					add !copy_offset slength;
					None
			in
			let rec loop pos =
				match step pos with
				| Some next ->
					if this.r_global then
						loop next
					else
						add !copy_offset slength
				| _ ->
					()
			in
			loop 0;
			encode_array (DynArray.to_list acc)
		end
	)
end

module StdFile = struct
	let create_out path binary flags =
		let path = decode_string path in
		let binary = match binary with
			| VTrue | VNull -> true
			| _ -> false
		in
		let perms = 0o666 in
		let l = Open_creat :: flags in
		let l = if binary then Open_binary :: l else l in
		let ch =
			try open_out_gen l perms path
			with Sys_error msg -> exc_string msg
		in
		encode_instance key_sys_io_FileOutput ~kind:(IOutChannel ch)

	let write_out path content =
		try
			let ch = open_out_bin path in
			output_string ch content;
			close_out ch;
			vnull
		with Sys_error s ->
			exc_string s

	let append = vfun2 (fun path binary ->
		create_out path binary [Open_append]
	)

	let update = vfun2 (fun path binary ->
		create_out path binary [Open_rdonly; Open_wronly]
	)

	let getBytes = vfun1 (fun path ->
		let path = decode_string path in
		try encode_bytes (Bytes.unsafe_of_string (Std.input_file ~bin:true path)) with Sys_error _ -> exc_string ("Could not read file " ^ path)
	)

	let getContent = vfun1 (fun path ->
		let path = decode_string path in
		try ((create_unknown (Std.input_file ~bin:true path))) with Sys_error _ -> exc_string ("Could not read file " ^ path)
	)

	let read = vfun2 (fun path binary ->
		let path = decode_string path in
		let binary = match binary with
			| VTrue | VNull -> true
			| _ -> false
		in
		let ch =
			try open_in_gen (Open_rdonly :: (if binary then [Open_binary] else [])) 0 path
			with Sys_error msg -> exc_string msg
		in
		encode_instance key_sys_io_FileInput ~kind:(IInChannel(ch,ref false))
	)

	let saveBytes = vfun2 (fun path bytes ->
		let path = decode_string path in
		let bytes = decode_bytes bytes in
		write_out path (Bytes.unsafe_to_string bytes)
	)

	let saveContent = vfun2 (fun path content ->
		let path = decode_string path in
		let content = decode_string content in
		write_out path content
	)

	let write = vfun2 (fun path binary ->
		create_out path binary [Open_wronly;Open_trunc]
	)
end

module StdFileInput = struct
	let raise_eof () =
		let v = encode_instance key_haxe_io_Eof in
		exc v

	let this vthis = match vthis with
		| VInstance {ikind = IInChannel(ch,eof)} -> ch,eof
		| _ -> unexpected_value vthis "FileInput"

	let close = vifun0 (fun vthis ->
		close_in (fst (this vthis));
		vnull
	)

	let eof = vifun0 (fun vthis ->
		vbool !(snd (this vthis))
	)

	let seek = vifun2 (fun vthis pos mode ->
		let ch,r = this vthis in
		r := false;
		let pos = decode_int pos in
		let mode,_ = decode_enum mode in
		seek_in ch (match mode with 0 -> pos | 1 -> pos_in ch + pos | 2 -> in_channel_length ch + pos | _ -> die "" __LOC__);
		vnull
	)

	let tell = vifun0 (fun vthis ->
		vint (pos_in (fst (this vthis)))
	)

	let readByte = vifun0 (fun vthis ->
		let ch,r = this vthis in
		let i = try
			input_char ch
		with _ ->
			r := true;
			raise_eof()
		in
		vint (int_of_char i)
	)

	let readBytes = vifun3 (fun vthis bytes pos len ->
		let ch,r = this vthis in
		let bytes = decode_bytes bytes in
		let pos = decode_int pos in
		let len = decode_int len in
		let i = input ch bytes pos len in
		if i = 0 then begin
			r := true;
			raise_eof()
		end;
		vint i
	)
end

module StdFileOutput = struct
	let this vthis = match vthis with
		| VInstance {ikind = IOutChannel ch} -> ch
		| _ -> unexpected_value vthis "FileOutput"

	let close = vifun0 (fun vthis ->
		close_out (this vthis);
		vnull
	)

	let flush = vifun0 (fun vthis ->
		flush (this vthis);
		vnull
	)

	let seek = vifun2 (fun vthis pos mode ->
		let this = this vthis in
		let pos = decode_int pos in
		let mode,_ = decode_enum mode in
		seek_out this (match mode with 0 -> pos | 1 -> pos_out this + pos | 2 -> out_channel_length this + pos | _ -> die "" __LOC__);
		vnull
	)

	let tell = vifun0 (fun vthis ->
		vint (pos_out (this vthis))
	)

	let writeByte = vifun1 (fun vthis c ->
		output_char (this vthis) (char_of_int (decode_int c));
		vnull
	)

	let writeBytes = vifun3 (fun vthis bytes pos len ->
		let this = this vthis in
		let bytes = decode_bytes bytes in
		let pos = decode_int pos in
		let len = decode_int len in
		output this bytes pos len;
		vint len
	)
end

module StdFPHelper = struct
	let doubleToI64 = vfun1 (fun v ->
		let f = num v in
		let i64 = Int64.bits_of_float f in
		EvalIntegers.encode_haxe_i64_direct i64
	)

	let floatToI32 = vfun1 (fun f ->
		let f = num f in
		let i32 = Int32.bits_of_float f in
		vint32 i32
	)

	let i32ToFloat = vfun1 (fun i ->
		let i32 = decode_i32 i in
		let f = Int32.float_of_bits i32 in
		vfloat f
	)

	let i64ToDouble = vfun2 (fun low high ->
		let low = decode_i32 low in
		let high = decode_i32 high in
		let b = Bytes.make 8 '0' in
		EvalBytes.write_i32 b 0 low;
		EvalBytes.write_i32 b 4 high;
		let i64 = EvalBytes.read_i64 b 0 in
		vfloat (Int64.float_of_bits i64)
	)
end

module StdFileSystem = struct
	let rec remove_trailing_slash p =
		let l = String.length p in
		if l = 0 then
			"" (* don't be retarded *)
		else match p.[l-1] with
			| '\\' | '/' -> remove_trailing_slash (String.sub p 0 (l - 1))
			| _ -> p

	let patch_path s =
		if String.length s > 1 && String.length s <= 3 && s.[1] = ':' then Path.add_trailing_slash s
		else if s = "/" then "/"
		else remove_trailing_slash s

	let createDirectory = vfun1 (fun path ->
		catch_unix_error Path.mkdir_from_path_unix_err (Path.add_trailing_slash (decode_string path));
		vnull
	)

	let deleteDirectory = vfun1 (fun path ->
		catch_unix_error Unix.rmdir (decode_string path);
		vnull
	)

	let deleteFile = vfun1 (fun path ->
		(try Sys.remove (decode_string path) with Sys_error s -> exc_string s);
		vnull
	)

	let exists = vfun1 (fun path ->
		let b = try Sys.file_exists (patch_path (decode_string path)) with Sys_error _ -> false in
		vbool b
	)

	let fullPath = vfun1 (fun relPath ->
		try create_unknown (Extc.get_full_path (decode_string relPath)) with exc -> exc_string (Printexc.to_string exc)
	)

	let isDirectory = vfun1 (fun dir ->
		let b = try Sys.is_directory (patch_path(decode_string dir)) with Sys_error _ -> false in
		vbool b
	)

	let readDirectory = vfun1 (fun dir ->
		let dir = decode_string dir in
		let d = try
			if not (Sys.is_directory (patch_path dir)) then exc_string "No such directory";
			Sys.readdir dir
		with Sys_error s ->
			exc_string s
		in
		encode_array (Array.to_list (Array.map (fun s -> create_unknown s) d))
	)

	let rename = vfun2 (fun path newPath ->
		(try Sys.rename (decode_string path) (decode_string newPath) with Sys_error s -> exc_string s);
		vnull
	)

	let stat = vfun1 (fun path ->
		let s = catch_unix_error Unix.stat (patch_path (decode_string path)) in
		encode_obj [
			key_gid,vint s.st_gid;
			key_uid,vint s.st_uid;
			key_atime,StdDate.encode_date s.st_atime;
			key_mtime,StdDate.encode_date s.st_mtime;
			key_ctime,StdDate.encode_date s.st_ctime;
			key_dev,vint s.st_dev;
			key_ino,vint s.st_ino;
			key_nlink,vint s.st_nlink;
			key_rdev,vint s.st_rdev;
			key_size,vint s.st_size;
			key_mode,vint s.st_perm;
		]
	)
end

module StdGc = struct
	open Gc
	let key_minor_heap_size = hash "minor_heap_size"
	let key_major_heap_increment = hash "major_heap_increment"
	let key_space_overhead = hash "space_overhead"
	let key_verbose = hash "verbose"
	let key_max_overhead = hash "max_overhead"
	let key_stack_limit = hash "stack_limit"
	let key_allocation_policy = hash "allocation_policy"
	let key_minor_words = hash "minor_words"