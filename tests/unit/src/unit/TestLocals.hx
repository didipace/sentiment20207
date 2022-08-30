package unit;

class TestLocals extends Test {

	function testIncrDecr() {
		var i = 5;
		eq( i++, 5 );
		eq( i, 6 );
		eq( i--, 6 );
		eq( i, 5 );
		eq( ++i, 6 );
		eq( i, 6 );
		eq( --i, 5 );
		eq( i, 5 );
	}

	function testScope() {
		var x = 0;
		eq(x,0);
		// simple scope
		{
			var x = "hello";
			eq(x,"hello");
			{
				var x = "";
				eq(x,"");
			}
			eq(x,"hello");
		}
		eq(x,0);
		// if
		var flag = true;
		if( flag ) {
			var x = "hello";
			eq(x,"hello");
		}
		eq(x,0);
		// for
		for( x in ["hello"] )
			eq(x,"hello");
		eq(x,0);
		// switch
		switch( MyEnum.D(MyEnum.A) ) {
		case D(x):
			eq(x,MyEnum.A);
		default:
			assert();
		}
		eq(x,0);
		// try/catch
		try {
			throw "hello";
		} catch( x : Dynamic ) {
			eq(x,"hello");
		}
		eq(x,0);
	}

	function testCapture() {
		// read
		var funs = new Array();
		for( i in 0...5 )
			funs.push(function() return i);
		for( k in 0...5 )
			eq(funs[k](),k);

		// write
		funs = new Array();
		var sum = 0;
		for( i in 0.