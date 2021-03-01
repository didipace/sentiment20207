package haxe;

import cs.system.Exception as CsException;
import cs.system.diagnostics.StackTrace;

@:coreApi
class Exception extends NativeException {
	public var message(get,never):String;
	public var stack(get,never):CallStack;
	public var previous(get,never):Null<Excep