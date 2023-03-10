// add, toString
var x = new StringBuf();
x.toString() == "";
x.add(null);
x.toString() == "null";

// addChar
var x = new StringBuf();
x.addChar(32);
x.toString() == " ";

// addSub
var x = new StringBuf();
x.addSub("abcdefg", 1);
x.toString() == "bcdefg";
var x = new StringBuf();
x.addSub("abcdefg", 1, null);
x.toString() == "bcdefg";
var x = new StringBuf();
x.addSub("abcdefg", 1, 3);
x.toString() == "bcd";

// surrogate characters
#if !(neko)
var x = new StringBuf();
x.add("ğ½");
x.toString() == "ğ½";
var x = new StringBuf();
x.addChar(0x1F47D);
x.toString() == "ğ½";
var x = new StringBuf();
#if utf16
x.addSub("ağ½b", 1, 2);
#else
x.addSub("ağ½b", 1, 1);
#end
x.toString() == "ğ½";
#end

// identity
function identityTest(s:StringBuf) {
	return s;
}
identityTest(x) == x;