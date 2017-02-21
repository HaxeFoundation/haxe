package unit.issues;

class Issue6043 extends unit.Test {
	function test() {
		var input = new haxe.io.StringInput("a\nb\n");
		var lineNo = 0;
		var line = "";
		try {
			while (true) {
				lineNo++;
				line = input.readLine();
			}
		} catch(ex:haxe.io.Eof) {
			t(true);
			return;
		}
		t(false);
	}
}