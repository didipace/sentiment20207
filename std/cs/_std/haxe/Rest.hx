package haxe;

import haxe.iterators.RestIterator;
import haxe.iterators.RestKeyValueIterator;
import cs.NativeArray;
import cs.system.Array as CsArray;

private typedef NativeRest<T> = #if erase_generics NativeArray<Dynamic> #else NativeArray<T> #end;

@:coreApi
abstract Rest<T>(NativeRest<T>) {
	public var length(get,never):Int;
	inline function get_length():Int
		return this.Length;

	@:from static public inline function of<T>(array:Array<T>):Rest<T>
		return new Rest(@:privateAccess array.__a);

	inline function new(a:NativeRest<T>):Void
		this = a;

	@:arrayAccess inline function get(index:Int):T
		return (this[index] : T); // typecheck, otherwise it will be inlined as Dynamic with `-D erase-generics`

	@:to public function toArray():Array<T> {
		var resul