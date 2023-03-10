
function traverse(s:String) {
	var a = [];
	for (code in new haxe.iterators.StringIteratorUnicode(s)) {
		a.push(code);
	}
	return a;
}

#if (target.unicode || neko)

traverse("abcde") == ["a".code, "b".code, "c".code, "d".code, "e".code];
traverse("aaðÃ©Ã©") == ["a".code, "a".code, "ð".code, "Ã©".code, "Ã©".code];

var surrogateBorders = [
	"ð", //D800,DC00 - U+10000
	"ð¿", //D800,DFFF - U+103FF
	"ô°", //DBFF,DC00 - U+10FC00
	"ô¿¿", //DBFF,DFFF - U+10FFFF
];
var rStr = traverse(surrogateBorders.join(''));
rStr == [
	65536,	//D800,DC00 - U+10000
	66559,	//D800,DFFF - U+103FF
	1113088,//DBFF,DC00 - U+10FC00
	1114111	//DBFF,DFFF - U+10FFFF
];

#else
1 == 1;
#end