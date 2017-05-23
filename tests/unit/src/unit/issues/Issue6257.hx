package unit.issues;

class Issue6257 extends unit.Test {
	function test() {
		var packages:Map<String, Map<String, String>> = ["" => new Map()];
		packages.get("").set("foo", "bar");
		var obj = {prop:"foo"};
		(obj.prop == "foo" ? obj : null).prop = "bar";
		eq("bar", packages.get("").get("foo"));
	}
}
