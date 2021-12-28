package unit.issues;
import unit.Test;
import haxe.ds.StringMap;

class Issue3462 extends Test
{
	function test()
	{
		var d:StringMap<Int> = new StringMap<Int>();
		var r:Dynamic;

		var s:String = "";
		var keys = [