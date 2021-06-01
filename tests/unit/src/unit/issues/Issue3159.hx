package unit.issues;

class Issue3159 extends Test
{
	public function test()
	{
		var dmet = new DynamicMethod();
		f(dmet.hasCalled);
		var test:IHasDynamicMethod = dmet;
		te