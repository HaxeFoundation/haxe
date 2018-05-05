package unit.issues;

class Issue4196 extends Test {
	function test() {
		var f = [
			'a' => {"id": { "deep" : 5 }},
			'b' => {"id": {}}
		];
		var a:haxe.ds.Map<String, {id:{}}>;
		unit.HelperMacros.typedAs(f, a);
	}
}