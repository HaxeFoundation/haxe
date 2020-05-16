package unit.issues;

private typedef Schema = {
	final "$ref":String;
	var ?"enum":Int;
	var "default":{"$id":Int, "class":String};
	var "aria-details":String;
}

class Issue7722 extends unit.Test {
	function test() {
		var a:Schema = {
			"$ref": "hi",
			"enum": 42,
			"default": {"$id": 10, "class": "bye"},
			"aria-details": "yep",
		};
		var b = {
			"$ref": "hi",
			"enum": 42,
			"default": {"$id": 10, "class": "bye"},
			"aria-details": "yep",
		};
		eq("hi", a."$ref");
		eq(42, a."enum");
		eq(10, a."default"."$id");
		eq("bye", a."default"."class");
		eq("yep", a."aria-details");
		eq("hi", b."$ref");
		eq(42, b."enum");
		eq(10, b."default"."$id");
		eq("bye", b."default"."class");
		eq("yep", b."aria-details");
	}
}
