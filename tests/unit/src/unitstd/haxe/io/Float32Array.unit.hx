var b = new haxe.io.Float32Array(5);
b[0] == 0;
b[4] == 0;
b.length == 5;

// check float write
b[1] = 1.25;
b[1] == 1.25;

// check loss of precision due to 32 bits
b[1] = 8589934591.;

#if typedarray_precision_check // see issue #7407
b[1] == 8589934592.;
#end


// set
for( i in 0...5 )
	b[i] = i + 1;
b[0] == 1;
b[4] == 5;

// access outside bounds is unspecified but should not crash
try b[-1] catch( e : Dynamic ) {};
try b[5] catch(e : Dynamic) {};

// same for writing
try b[-1] = 55 catch( e : Dynamic ) {};
try b[5] = 55 catch(e : Dynamic) {};

var b2 = b.sub(1,3);
b2[0] == 2;
b2[2] == 4;
b2.length == 3;

// check memory sharing
b2[0] = 0xCC;
b2[0] == 0