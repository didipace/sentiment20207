/*
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
 */

package php.db;

import haxe.extern.*;
import php.*;
import haxe.Constraints.Function;

/**
	@see http://php.net/manual/en/class.mysqli.php
**/
@:native('Mysqli')
extern class Mysqli {
	var affected_rows(default, null):Int;
	var client_info(default, null):String;
	var client_version(default, null):Int;
	var connect_errno(default, null):Int;
	var connect_error(default, null):String;
	var errno(default, null):Int;
	var error_list(default, null):NativeAssocArray<Scalar>;
	var error(default, null):String;
	var field_count(default, null):Int;
	var host_info(default, null):String;
	var protocol_version(default, null):String;
	var server_info(default, null):String;
	var server_version(default, null):Int;
	var info(default, null):String;
	var insert_id(default, null):EitherType<Int, String>;
	var sqlstate(default, null):String;
	var thread_id(default, null):Int;
	var warning_count(default, null):Int;

	static function poll(read:Ref<NativeArray>, error:Ref<NativeArray>, reject:Ref<NativeArray>, sec:Int, ?usec:Int):Int;

	function new(?host:String, ?username:String, ?passwd:String, dbname:String = "", ?port:Int, ?socket:String):Void;
	function autocommit(mode:Bool):Bool;
	function begin_transaction(?flags:Int, ?name:String):Bool;
	function change_user(user:String, password:String, database:String):Bool;
	function character_set_name():String;
	function close():Bool;
	function commit(?flags:Int, ?name:String):Bool;
	function debug(message:String):Bool;
	function dump_debug_info():Bool;
	function get_charset():{
		charset:String,
		collation:String,
		dir:String,
		min_length:Int,
		number:Int,
		state:Int
	};
	function get_client_info():String;
	function get_connection_stats():Bool;
	function get_warnings():Mysqli_warning;
	function init():Mysqli;
	function kill(processid:Int):Bool;
	function more_results():Bool;
	function multi_query(query:String):Bool;
	function next_result():Bool;
	function options(option:Int, value:Scalar):Bool;
	function ping():Bool;
	function prepare(query:String):Mysqli_stmt;
	function query(query:String, ?resultmode:Int):EitherType<Bool, Mysqli_result>;
	function real_connect(?host:String, ?username:String, ?passwd:String, ?dbname:String, ?port:Int, ?socket:String, ?flags:Int):Bool;
	function escape_string(escapestr:String):String;
	function real_query(query:String):Bool;
	function r