package unit;

class TestBasetypes extends Test {

	function testArray() {
		var a : Array<Null<Int>> = [1,2,3];
		eq( a.length, 3 );
		eq( a[0], 1 );
		eq( a[2], 3 );

		eq( a[3], null );
		eq( a[1000], null );
		eq( a[-1], null );

		a.remove(2);
		eq( a.length, 2);
		eq( a[0], 1 );
		eq( a[1], 3 );
		eq( a[2], null );

		var a : Array<Null<Int>> = [1,2,3];
		a.splice(1,1);
		eq( a.length, 2 );
		eq( a[0], 1 );
		eq( a[1], 3 );
		eq( a[2], null );
	}

	function testString() {
		eq( String.fromCharCode(77), "M" );
		unspec(function() String.fromCharCode(0));
		unspec(function() String.fromCharCode(-1));
		unspec(function() String.fromCharCode(256));

		eq( null + "x", "nullx" );
		eq( "x" + null, "xnull" );

		var abc = "abc".split("");
		eq( abc.length, 3 );
		eq( abc[0], "a" );
		eq( abc[1], "b" );
		eq( abc[2], "c" );

		var str = "abc";
		eq( str.charCodeAt(0), "a".code );
		eq( str.charCodeAt(1), "b".code );
		eq( str.charCodeAt(2), "c".code );
		eq( str.charCodeAt(-1), null );
		eq( str.charCodeAt(3), null );

		// substr tests
		var sentence:String = "Pack my box with five dozen liquor jugs.";
		eq(sentence.substr(0, 4), "Pack");
		eq(sentence.substr(5, 2), "my");
		eq(sentence.substr(0), sentence);
		eq(sentence.substr(35), "jugs.");
		eq(sentence.substr(40), "");
		eq(sentence.substr(42), "");
		eq(sentence.substr(-5, 4), "jugs");
		eq(sentence.substr(-5), "jugs.");
		eq(sentence.substr(-42), sentence);
		eq(sentence.substr(4, 0), "");
		eq(sentence.substr(0, -36), "Pack");

		// null should not be swallowed
		eq("hello" +null, "hellonull");
		eq(null + "hello", "nullhello");
		var x:Dynamic = null;
		//String const + Dynamic var with null ref
		eq("hello" +x, "hellonull");
		eq(x + "hello", "nullhello");
		var y:Dynamic = "hello";
		//Dynamic var + Dynamic var, where one is null, the other is a string:
		eq(x + y, "nullhello");
		eq(y + x, "hellonull");
		var x:String = null;
		//String const + String var with null ref
		eq("hello" +x, "hellonull");
		eq(x + "hello", "nullhello");

		var x = { hello:"world", val:5 };
		var xs = "" + x;
		// Output should contain hello followed by world, and val followed by 5.
		// The order of fields and operator between key and value remain unspecified.
		var h = xs.indexOf("hello");
		t(h != -1);
		t(xs.indexOf("world", h) != -1);
		h = xs.indexOf("val");
		t(h != -1);
		t(xs.indexOf("5", h) != -1);
		eq(x + "", xs);

		// Let's just make sure this is not 10 on any platform.
		eq(5 + "5", "55");
		eq("5" + 5, "55");
		eq("5" + 5.1, "55.1");

		// Some precedence checks.
		eq(1 + 1 + 1 + 1 + "1", "41");
		eq("1" + 1 + 1 + 1 + 1, "11111");
		eq(1 + 1 + "1" + 1 * 2, "212");

		// check recursive formating
		var x = [[1], [2, 3]];
		eq("" + x, "[[1],[2,3]]");

		// Brackets around array values should not be stripped.
		var x : Array<Dynamic> = [1, "hello"];
		eq("" + x, "[1,hello]");
		eq(x + "", "" + x);

		// This is also true for iterables that are arrays.
		var x:Iterable<Dynamic> = x;
		eq("" + x, "[1,hello]");
		eq(x + "", "" + x);

		// I don't think this should throw an exception on PHP.
		try {
			"" + x.iterator();
		} catch (e:Dynamic)	{
			assert("Could not convert Iterator to String");
		}

		var str = "he\nlo\"'";
		eq( Std.string(str), str);
		eq( Std.string([str]), "[" + str + "]");

		var e = MyEnum.C(0, "h");
		eq( Std.string(e), "C(0,h)");

		eq(Std.string([e]), "[C(0,h)]");

		var tester:String = "show me the (show me!) index of show me";
		eq(tester.lastIndexOf("show me"), 32);
		eq(tester.lastIndexOf("show me", 1), 0);
		eq(tester.lastIndexOf("show m