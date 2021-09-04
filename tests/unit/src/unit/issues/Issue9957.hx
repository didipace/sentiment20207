package unit.issues;

class Issue9957 extends unit.Test {
	function test() {
		var stat = staticExc;
		var inst = instanceExc;

		try {
			try stat()
		