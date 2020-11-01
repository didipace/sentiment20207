package haxe.atomic;

import cs.system.threading.Interlocked.*;

private class ObjectWrapper<T:{}> {
	public var value:T;

	public function new(value:T) {
		this.value = value;
	}
}

extern abstract AtomicObject<T:{}>(ObjectWrapper<T>) {
	public inline function new(value:T) {
		this = new ObjectWrapper(value);
	}

	public inline function compareExchange(expected:T, replacement:T):T {
		return cs.Syntax.code("System.Threading.Interlocked.CompareExchange(ref ({0