package unit.issues;

@:enum // comment to make it work
private abstract TextFormatAlign(String) {
	var Center = "center";
	@:from static function fromString(value:String):TextFormatAlign {
		return cast value;
	}
}

class Issue6656 extends unit.Test {
	function test() {
		function getAlign() return "center";
		var a = (getAlign() : TextFormatAlign); // String should be TextFormatAlign
		eq(a, Center);
	}
}

