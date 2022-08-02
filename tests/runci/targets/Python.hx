package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

class Python {
	static public function getPythonDependencies():Array<String> {
		switch (systemName) {
			case "Linux":
				if (commandSucceed("python3", ["-V"]))
					infoMsg('python3 has already 