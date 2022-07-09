/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package sys.thread;

private typedef ThreadImpl = HaxeThread;

abstract Thread(ThreadImpl) from ThreadImpl {

	public var events(get,never):EventLoop;

	public inline function sendMessage(msg:Dynamic):Void {
		this.sendMessage(msg);
	}

	public static inline function current():Thread {
		return HaxeThread.current();
	}

	public static inline function create(job:()->Void):Thread {
		return HaxeThread.create(job, false);
	}

	public static inline function runWithEventLoop(job:()->Void):Void {
		HaxeThread.runWithEventLoop(job);
	}

	public static inline function createWithEventLoop(job:()->Void):Thread {
		return HaxeThread.create(job, true);
	}

	public static inline function readMessage(block:Bool):Dynamic {
		return HaxeThread.readMessage(block);
	}

	function get_events():EventLoop {
		if(this.events == null)
			throw new NoEventLoopException();
		return this.events;
	}

	@:keep
	static function processEvents() {
		HaxeThread.current().events.loop();
	}
}

@:callable
@:coreType
private abstract NativeThreadHandle {}

private typedef ThreadHandle = NativeThreadHandle;

private class HaxeThread {
	static var thread_create:(callb:(_:Dynamic)->Void, _:Dynamic)->ThreadHandle;
	static var thread_current:()->ThreadHandle;
	static var thread_send:(handle:ThreadHandle, msg:Dynamic)->Void;
	static var thread_read_message:(block:Bool)->Dynamic;

	static var mainThreadHandle:ThreadHandle;
	static var mainThread:HaxeThread;

	static var threads:Array<{thread:HaxeThread, handle:ThreadHandle}>;
	static var threadsMutex:Mutex;

	static function __init__() {
		thread_create = neko.Lib.load("std", "thread_create", 2);
		thread_current = neko.Lib.load("std", "thread_current", 0);
		thread_send = neko.Lib.load("std", "thread_send", 2);
		thread_read_message = neko.Lib.load("std", "thread_read_message", 1);

		mainThreadHandle = thread_current();
		mainThread = new HaxeThread(mainThreadHandle);
		mainThread.events = new EventLoop();

		threads = [];
		threadsMutex = new Mutex();
	}

	public var events(default,null):Null<EventLoop>;
	public var handle:ThreadHandle;

	static public function current():HaxeThread {
		var handle = thread_current();
		if(handle == mainThreadHandle) {
			return mainThread;
		}
		threadsMutex.acquire();
		var thread = null;
		for(item in threads) {
			if(item.handle == handle) {
				thread = item.thread;
				break;
			}
		}
		if(thread == null) {
			thread = new HaxeThread(handle);
			threads.push({thread:thread, handle:handle});
		}
		threadsMutex.release();
		return thread;
	}

	public static function create(callb:()->Void, withEventLoop:Bool):Thread {
		var item = {handle:null, thread:new HaxeThread(null)};
		threadsMut