package unit.issues;

private typedef Schema = {
	final "$ref":String;
	var ?"enum":Int;
	var "default":{"$id":Int, "class":String};
}

class Issue7722 extends unit.Test {
	function test() {
		var a:Schema = {
			"$ref": "hi",
			"enum": 42,
			"default": {"$id": 10, "class": "bye"}
		};
		var b = {
			"$ref": "hi",
			"enum": 42,
			"default": {"$id": 10, "class": "bye"}
		};
		eq("hi", a."$ref");
		eq(42, a."enum");
		eq(10, a."default"."$id");
		eq("bye", a."default"."class");
		eq("hi", b."$ref");
		eq(42, b."enum");
		eq(10, b."default"."$id");
		eq("bye", b."default"."class");
	}
}
