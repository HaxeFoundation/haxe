package unit.issues;

class Issue8210 extends Test {
	#if eval
	function test() {
		var p = new sys.io.Process("haxe_issue_8210_does_not_exist", []);
		eq(127, p.exitCode());
		var err = p.stderr.readAll().toString();
		t(err.indexOf("haxe_issue_8210_does_not_exist") >= 0);
		p.close();
	}
	#end
}
