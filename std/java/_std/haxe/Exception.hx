package haxe;

import java.NativeArray;
import java.lang.Throwable;
import java.lang.RuntimeException;
import java.lang.StackTraceElement;
import java.io.PrintStream;
import java.io.PrintWriter;

@:coreApi
class Exception extends NativeException {
	public var message(get,never):String;
	public var stack(get,never):CallStack;
	public var previous(get,never):Null<Exception>;
	public var native(get,never):Any;

	@:noCompletion var __exceptionStack:Null<CallStack>;
	@:noCompletion var __nativeException:Throwable;
	@:noCompletion var __previousException:Null<Exception>;

	static function caught(value:Any):Exception {
		if(Std.isOfType(value, Exception)) {
			return value;
		} else if(Std.isOfType(value, Throwable)) {
			return new Exception((value:Throwable).getMessage(), null, value);
		} else {
			return new ValueException(value, null, value);
		}
	}

	static function thrown(value:Any):Any {
		if(Std.isOfType(value, Exception)) {
			var native = (value:Exception).__nativeException;
			return Std.isOfType(native, RuntimeException) ? native : value;
		} else if(Std.isOfType(value, RuntimeException)) {
			return value;
		} else if(Std.isOfType(value, Throwable)) {
			return new Exception((value:Throwable).getMessage()