package cases;

@:timeout(2000)
class TestEvents extends utest.Test {

	function testIssue10567_runEventsInOrderByTime(async:Async) {
		var events = Thread.current().events;
		var checks = [];
		var e3 = null;
		var e2 = null;
		var e1 = null;
		e2 = events.repeat(() -> {
			checks.push(2);
			events.cancel(e1);
			events.cancel(e2);
			events.cancel(e3);
		}, 20);
		e1 = events.repeat(() -> checks.push(1), 10);
		e3 = events.repeat(() -> checks.push(3), 30);
		Sys.sleep(0.1);

		var checker = null;
		checker = events.repeat(() -> {
			same([1, 2], checks);
			async.done();
			events.cancel(checker);
		}, 100);
	}

	f