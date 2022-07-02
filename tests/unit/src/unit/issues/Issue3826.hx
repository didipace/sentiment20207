package unit.issues;

class Issue3826 extends Test {

	var inull : Null<Int> = null;
	var fnull : Null<Float> = null;
	var dnull : Dynamic = null;
	var ival : Null<Int> = 5;
	var fval : Null<Float> = 8.5;
	var dval : Dynamic = 6;

	function get( a = 2, b = 4.25 ) {
		return ""+a+"/"+b;
	}

	public function test() {
		eq( get(), "2/4.25" );
		eq( get(5), "5/4.25" );
		eq( get(5,8.5), "5/8.5" );

		#if !flash
		// flash does have two problems wrt these tests
		// - not skippable nullable arg
		// - explicit null args are cast to 0
		eq( get(8.5), "2/8.5" );

		eq( get(inull), "2/4.25" );
		eq( get(inull,fnull), "2/4.25" );
		eq( get(inull,inull), "2/4.25" );
		eq( get(fnull), "2/4.25" );
		eq( get(dnull,dnull), "2/4.25" );
		eq( get(ival,fval), "5/8.5" );
		eq( get(dval,dval), "6/6" );

		#end
	}

	#if !((java && !jvm) || cs || flas