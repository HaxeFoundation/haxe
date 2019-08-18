package unit.issues;

class Issue6674 extends unit.Test {
	var someArray = [];

	function test() {
		load(function(response) {
			response["foo"];
			someArray.push(1);
		});
		eq(1, someArray.length);
		eq(1, someArray[0]);
	}

	@:overload(function(cb:haxe.DynamicAccess<String> -> Void):Void {})
	static public function load(cb:String -> Void) {
		cb("");
	}
}