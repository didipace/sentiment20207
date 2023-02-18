haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog"), true) == "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==";
haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog"), false) == "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw";
haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog"), true) == "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==";
haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog"), false) == "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw";

haxe.crypto.Base64.decode("VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==", true).toString() == "The quick brown fox jumps over the lazy dog";
haxe.crypto.Base64.decode("VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw", false).toString() == "The quick brown fox jumps over the lazy dog";
haxe.crypto.Base64.urlDecode("VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==", true).toString() == "The quick brown fox jumps over the lazy dog";
haxe.crypto.Base64.urlDecode("VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw", false).toString() == "The quick brown fox jumps over the lazy dog";

haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("a"), true) == "YQ==";
haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("ab"), true) == "YWI=";
haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("ab?"), true) == "YWI/";
haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("ab~c"), true) == "YWJ+Yw==";
haxe.