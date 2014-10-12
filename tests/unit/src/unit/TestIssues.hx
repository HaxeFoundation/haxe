package unit;

using StringTools;

class TestIssues {
	macro static public function addIssueClasses() {
		var dir = sys.FileSystem.readDirectory("src/unit/issues");
		var el = [];
		function add(className:String) {
			el.push(macro classes.push(new unit.issues.$className()));
		}
		for (file in dir) {
			if (!file.endsWith(".hx")) {
				continue;
			}
			add(file.substr(0, -3));
		}
		return macro $b{el};
	}
}
