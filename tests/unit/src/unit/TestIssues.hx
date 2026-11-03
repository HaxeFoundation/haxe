package unit;

using StringTools;

class TestIssues {
	macro static public function addIssueClasses(dir:String, pack:String) {
		var dir = sys.FileSystem.readDirectory(dir);
		var el = [];
		function add(className:String) {
			var tp = {
				pack: pack.split("."),
				name: className,
				sub: null,
				params: null
			}
			el.push(macro classes.push(new $tp()));
		}
		for (file in dir) {
			if (!file.endsWith(".hx")) {
				continue;
			}
			add(file.substr(0, -3));
		}
		return macro $b{el};
	}

	macro static public function addTestClasses(dir:String, pack:String) {
		var el = [];
		function readDir(dir:String, pack:String) {
			for (file in sys.FileSystem.readDirectory(dir)) {
				var path = dir + "/" + file;
				if (sys.FileSystem.isDirectory(path)) {
					readDir(path, pack + "." + file);
				} else if (file.endsWith(".hx") && file.startsWith("Test")) {
					var className = file.substr(0, -3);
					var tp = {
						pack: pack.split("."),
						name: className,
						sub: null,
						params: null
					}
					el.push(macro classes.push(new $tp()));
				}
			}
		}
		readDir(dir, pack);
		return macro $b{el};
	}
}
