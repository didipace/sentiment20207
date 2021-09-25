package haxe;

//TODO: extend ::std::exception
@:coreApi
class Exception {
	public var message(get,never):String;
	public var stack(get,never):CallStack;
	public var previous(get,never):Null<Exception>;
	public var native(get,never):Any;

	@:noCompletion var __exceptionMessage:String;
	@:noCompletion var __exceptionStack:Null<CallStack>;
	@:noCompletion var __nativeStack:Array<String>;
	@:noCompletion @:ifFeature("haxe.Exception.get_stack") var __skipStack:Int = 0;
	@:noCompletion var __nativeException:Any;
	@:noCompletion var __previousException:Null<Exception>;

	static function caught(value:Any):Exception {
		if(Std.isOfType(value, Exception)) {
			return value;
		} else {
			return new ValueException(value, null, value);
		}
	}

	static function thrown(value:Any):Any {
		if(Std.isOfType(value, Exception)) {
			return (value:Exception).native;
		} else {
			var e = new ValueException(value);
			e.__shiftStack();
			return e;
		}
	}

	public function new(message:String, ?previous:Exception, ?native:Any) {
		__exceptionMessage = message;
		__previousException = previous;
		if(native != null) {
			__nativeStack = NativeStackTrace.exceptionStack();
			__nativeExce