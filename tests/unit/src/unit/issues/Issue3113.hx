package unit.issues;

class Issue3113 extends Test
{
	public function test()
	{
		var t:TD = cast A;
		eq( t.func(1, 2), 1 ); // works

		t = cast B;
		eq( t.func(1, 2), 1 ); // runtime: Unhandled Exception:
							   // Haxe Exception: Method 'func' not found on type B
							   // [ERROR]