class RunExe {


static function main() {
		/*
		cpp: cpp/Test-debug
		cs : cs/bin/cs
		cs_unsafe : cs_unsafe/bin/cs_unsafe
		*/
		var rel_path = "";
		if (neko.Web.isModNeko) {
			//mod neko: use get string
			rel_path = neko.Web.getParamsString();
			//display similar 