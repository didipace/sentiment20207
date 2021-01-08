package unit;

class TestLua extends Test {
	function testMultiReturnWrap(){
		var multi : Multi = untyped MultiCall.doit();
		var l = lua.Lua.type(multi);
		// test that the original multi variable was wrapped
		eq(lua.Lua.type(untyped __lua__("multi")), "table");
		eq(l, "table");
	}

	function testMultiReturnPlainFunctionCall(){
		var multi : Multi = untyped MultiCall.doit();
		// this shouldn't box the result
		eq(untyped __lua__("multi"), null);
	}

	function testMultiReturnValue(){
		var multi : Multi = untyped MultiCall.doit();
		var l = lua.Lua.type(multi.b);
		// test that the original multi wrapped variable was never created
		eq(untyped __lua__("multi"), null);
		eq(l, "string");
	}

	function testMultiReturnArgument(){
		// make sure multireturns passed as arguments are wrapped
		eq(MultiCall.acceptMr(untyped MultiCall.doit()), true);
	}

	function testMultiReturnValueHandled(){
		// make sure that multireturn is not wrapped if return values not used
		var old_hx_box_mr = untyped _hx_box_mr;
		var called = false;
		untyped _hx_box_mr = function(){
			called = true;
		}
		var k = lua.NativeStringTools.find("foo bar", "foo");
		eq(called,false);
		called = true;
		// make sure that multireturn is wrapped if return value