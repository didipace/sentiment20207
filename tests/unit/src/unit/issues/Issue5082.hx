package unit.issues;

private enum E {
	Tiles(x:Dynamic);
	TileInstances(x:Dynamic);
}

@:keep
private class Content {
	public var find:String -> Content;
	public function new() {
		find = function(s:String) return this;
	}

	public function spectrum(s:String, d:Dynamic) {
		return this;
	}

	public function closest(s:String) {
		return this;
	}

	public function css(s:Dynamic) {
		return "foo";
	}
}

class Issue5082 extends unit.Test {
	function test() {