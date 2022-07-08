package cases;

class Issue10412 extends DisplayTestCase {
	/**
		class Main {
		static function main() {
			new Game();
		}
		}

		class Scene {
		public var scale:Float;
		}

		class Game extends Scene {

		public function new():Void {}

		function itWorks():Void {
			// There is completion:
			// Res.|.bar;
			Res.foo.bar;
		}

		function itDoesNot():Void {
			// There is not:
			// Res.|.bar;
			Res.{-1-}.bar;

			// because of `final scale`
			// changing `final scale` to other name fixes it
			var strangeBust = 5 / 10;
			final scale = 5 / 10;
			// if you comment out `strangeBust` it will also work,
			// but not if you change `final scale` to `var scale`
		}
		}

		class Res {
		public static var foo = {bar: "hello"};
		}
	**/
	function testOriginal() {
		var fiel