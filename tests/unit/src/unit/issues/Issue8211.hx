package unit.issues;

class Issue8211 extends unit.Test {
	function test() {
		t(StringTools.endsWith("abc", "bc"));
		t(StringTools.endsWith("aňa", "ňa"));
		t(StringTools.endsWith("\u{1F604}\u{1F619}", "\u{1F604}\u{1F619}"));
	}
}