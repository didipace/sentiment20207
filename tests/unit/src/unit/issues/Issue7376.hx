package unit.issues;

import utest.Assert;

class Issue7376 extends unit.Test {
	function testTryCatch() {
		foo(bool -> {
			try {
				intJob();
			} catch(e:Dynamic) {
				voidJob();
			}
		});

		var fn = bool -> {
			try {
				intJob();
			} catch(e:Dynamic) {
				voidJob();
			}
		}
		foo(fn);

		var fn = bool -> {
			try {
				intJob();
			} catch(e:Dynamic) {
				return;
			}
		}
		foo(fn);

		noAssert();
	}

	function testSwitch() {
		foo(bool -> switch bool {
			case true: intJob();
			case false:
	