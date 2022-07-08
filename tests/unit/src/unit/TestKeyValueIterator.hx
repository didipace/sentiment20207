package unit;

using unit.TestKeyValueIterator.BoolExtension;

import haxe.ds.StringMap;

private class MyStringMap<T> {
	var map:StringMap<T>;
	public function new() {
		map = new StringMap();
	}

	public function set(key:String, value:T) {
		map.set(key, value);
	}

	public function keyValueIterator():KeyValueIterator<String, T> {
		var a = [];
		for (key in map.keys()) {
			a.push({key: key, value: map.get(key)});
		}
		a.sort((a, b) -> Reflect.compare(a.key, b.key));
		return a.iterator();
	}
}

private class MyWeirdIterator {
	var theresMore:Bool;
	public function new() {
		theresMore = true;
	}

	public function hasNext() return theresMore;
	public function next() {
		theresMore = false;
		return {key: 1, value: "foo"};
	}
}

private class MyNotIterable {
	public function new() { }

	public function keyValueIterator() return 1;
}

private class MyNotIterator {
	public function new() { }

	public function hasNext() return 1;
	public function next() {
		return {key: 1, value: "foo"};
	}
}

class BoolExtension {
	static public function keyValueIterator(b:Bool) {
		return [{key: b, value: b}].iterator();
	}
}

class TestKeyValueIterator extends Test {
	function testIterable() {
		var map = new MyStringMap();
		map.set("1", "foo");
		map.set("2", "bar");
		var buf = new StringBuf();
		for (key => value in map) {
			buf.add(key);
			buf.add(value);
			buf.add(";");
		}
		eq("1foo;2