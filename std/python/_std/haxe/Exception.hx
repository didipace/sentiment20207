package haxe;

import python.Exceptions.BaseException;
import python.Exceptions.Exception in PyException;
import python.lib.Traceback;
import python.internal.UBuiltins;

private typedef PyStackItem = python.Tuple.Tuple4<String, Int, String, String>;

@:coreApi
class Exception extends PyException {
	public var message(get,never):String;
	public var stack(get,never):CallStack;
	public var previous(get,never):Null<Exception>;
	public var native(get,never):Any;

	@:noCompletion var __exceptionStack:Null<CallStack>;
	@:noCompletion var __nativeStack:Array<PyStackItem>;
	@:noCompletion @:ifFeature("haxe.Exception.get_stack") var __skipStack:Int = 0;
	@:noCompletion var __nativeException:BaseException;
	@:noCompletion var __previousException:Null<Exception>;

	static function caught(value:Any):Exception {
		if(Std.isOfType(value, Exception)) {
			return value;
		} else if(Std.isOfType(value, BaseException)) {
			return new Exception(UBuiltins.str(value), null, value);
		} else {
			return new ValueException(value, null, value);
		}
	}

	static function thrown(value:Any):Any {
		if(Std.isOfType(value, Exception)) {
			return (value:Exception).native;
		} else if(Std.isOfType(value, BaseExcepti