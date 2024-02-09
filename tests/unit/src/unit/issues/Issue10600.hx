package unit.issues;

class Issue10600 extends Test {
	function test() {
		var x = {
			"checkstyle.checks.Checker": ["CheckBase", "UnusedImportCheck"]
		};
		var a = Reflect.field(x, "checkstyle.checks.Checker");
		eq("CheckBase", a[0]);
		eq("UnusedImportCheck", a[1]);
	}
}
