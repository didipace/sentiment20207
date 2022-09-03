
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

import java.internal.Function;
import java.internal.HxObject;
import java.internal.Runtime;
import java.Boot;

@:coreApi class Reflect {
	public static function hasField(o:Dynamic, field:String):Bool {
		if (Std.isOfType(o, IHxObject)) {
			return untyped (o : IHxObject).__hx_getField(field, false, true, false) != Runtime.undefined;
		}
		return Runtime.slowHasField(o, field);
	}

	@:keep
	public static function field(o:Dynamic, field:String):Dynamic {
		if (Std.isOfType(o, IHxObject)) {
			return untyped (o : IHxObject).__hx_getField(field, false, false, false);
		}
		return Runtime.slowGetField(o, field, false);
	}

	@:keep
	public static function setField(o:Dynamic, field:String, value:Dynamic):Void {
		if (Std.isOfType(o, IHxObject)) {
			untyped (o : IHxObject).__hx_setField(field, value, false);
		} else {
			Runtime.slowSetField(o, field, value);
		}
	}

	public static function getProperty(o:Dynamic, field:String):Dynamic {
		if (o == null || field == null) {
			return null;
		}
		if (Std.isOfType(o, IHxObject)) {
			return untyped (o : IHxObject).__hx_getField(field, false, false, true);
		}
		if (Runtime.slowHasField(o, "get_" + field)) {
			return Runtime.slowCallField(o, "get_" + field, null);
		}
		return Runtime.slowGetField(o, field, false);
	}

	public static function setProperty(o:Dynamic, field:String, value:Dynamic):Void {
		if (Std.isOfType(o, IHxObject)) {
			untyped (o : IHxObject).__hx_setField(field, value, true);
		} else if (Runtime.slowHasField(o, "set_" + field)) {
			Runtime.slowCallField(o, "set_" + field, java.NativeArray.make(value));
		} else {
			Runtime.slowSetField(o, field, value);
		}
	}

	public static function callMethod(o:Dynamic, func:haxe.Constraints.Function, args:Array<Dynamic>):Dynamic {
		var args = java.Lib.nativeArray(args, true);
		return untyped (func : Function).__hx_invokeDynamic(args);
	}
