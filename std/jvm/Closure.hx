package jvm;

import java.NativeArray;
import java.lang.reflect.Method;

@:native("haxe.jvm.Closure")
@:nativeGen
@:keep
class Closure extends ClosureDispatch {
	public var context:Dynamic;
	public var method:Method;

	var isStatic:Bool;
	var params:NativeArray<java.lang.Class<Dynamic>>;

	public function new(context:Null<Dynamic>, method:Method) {
		super();
		this.context = context;
		this.method = method;
		isStatic = method.getModifiers() & java.lang.reflect.Modifier.STATIC != 0;
		params = method.getParameterTypes();
	}

	public function bindTo(context:Dynamic) {
		return new Closure(context, method);
	}

	override public function equals(other:java.lang.Object) {
		if (!Jvm.instanceof(other, Closure)) {
			return false;
		}
		var other:Closure = cast other;
		return context == other.context && method == other.method;
	}

	public override function invokeDynamic(args:NativeArray<Dynamic>):Dynamic {
		if (isStatic && context != null) {
			var newArgs = new NativeArray(args.length + 1)