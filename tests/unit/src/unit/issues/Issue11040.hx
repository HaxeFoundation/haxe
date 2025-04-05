package unit.issues;

class Issue11040 extends Test {
	function run() {
		var tagName = "Array";
		var text:String = "<Array<haxe.Json>>";
		var startTag = '<$tagName';
		var endTag = '</$tagName>';

		var depth = 0;
		var index = 0;
		var buf = new StringBuf();
		function append(s:String) {
			buf.add(s);
			buf.add("\n");
		}
		append("enter loop");
		while (true) {
			append("looping");
			var indexStartTag = text.indexOf(startTag, index);
			var indexEndTag = text.indexOf(endTag, index);
			if ((indexStartTag == -1) && (indexEndTag == -1)) {
				append(">>> should exit here >>>");
				return buf.toString();
			}
			index++;
			switch (text.charAt(index)) {
				case " " | "/" | ">":
				default:
					append("default -> continue loop");
					continue;
			}
			if (depth <= 0) {
				break;
			}
		}
		text = text.substr(0, index);
		append("exit loop");
		return buf.toString();
	}

	function test() {
		eq("enter loop
looping
default -> continue loop
looping
>>> should exit here >>>
", run());
	}
}
