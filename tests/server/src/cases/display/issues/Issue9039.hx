package cases.display.issues;

class Issue9039 extends DisplayTestCase {
	function test(_) {
		vfs.putContent("I.hx", "interface I { var prop(get,never):Int; }");
		vfs.putContent("Main.hx", "class Main { static function main() { var i:I = null; } }");

		//TODO: Regression. It started to fail without `--interp` on 2020-05-04
		runHaxe(["--no-output", "-main", "Main", "--interp"]);

		var content = "class Main { static function main() { var i:I = null; i.{-1-} } }";
		var transfor