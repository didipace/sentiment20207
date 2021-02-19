import sys.FileSystem;
import utest.Assert;
using StringTools;

class TestFileSystem extends utest.Test {
	/**
		Recursively remove a given directory.
	*/
	static function removeDir(dir:String):Void {
		if (FileSystem.exists(dir)) {
			for (item in FileSystem.readDirectory(dir)) {
				item = haxe.io.Path.join([dir, item]);
				if (FileSystem.isDirectory(item)) {
					removeDir(item);
				} else {
					FileSystem.deleteFile(item);
				}
			}
			FileSystem.deleteDirectory(dir);
		}
	}

	var dir = "temp/TestFileSystem/";
	var tailingSlashes = switch (Sys.systemName()) {
		case "Windows": ["", "/", "\\"];
		case _: ["", "/"];
	}

	public function setup() {
		removeDir(dir);
		FileSystem.createDirectory(dir);
	}

	public function teardown() {
		removeDir(dir);
	}

	function testReadDirectory():Void {
		for (tailingSlash in tailingSlashes) {
			Assert.equals(0, FileSystem.readDirectory(dir).length);
			for (name in FileNames.names) {
				var path = dir + name + tailingSlash;
				FileSystem.createDirectory(path);
				var files = 