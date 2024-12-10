package unit.issues;

using Reflect;

class Issue6276 extends unit.Test {
	function test(){
		var s = "bar";

		var length = s.field("length");
		eq(3, length);

		var charAt = s.field("charAt");
		eq(s.charAt(0), Reflect.callMethod(s, charAt, [0]));

		var charCodeAt = s.field("charCodeAt");
		eq(s.charCodeAt(0), Reflect.callMethod(s, charCodeAt, [0]));

		var indexOf = s.field("indexOf");
		eq(s.indexOf("a"), Reflect.callMethod(s, indexOf, ["a"]));

		var lastIndexOf = s.field("lastIndexOf");
		eq(s.lastIndexOf("r"), Reflect.callMethod(s, lastIndexOf, ["r"]));

		var split = s.field("split");
		eq(s.split("a")[1], Reflect.callMethod(s, split, ["a"])[1]);

		var substr = s.field("substr");
		eq(s.substr(1),Reflect.callMethod(s, substr, [1, 2]));

		var substring = s.field("substring");
		eq(s.substring(0, 2), Reflect.callMethod(s, substring, [0, 2]));

		var toLowerCase = s.toUpperCase().field("toLowerCase");
		eq(s, Reflect.callMethod(s, toLowerCase, []));

		var toUpperCase = s.field("toUpperCase");
		eq(s.toUpperCase(), Reflect.callMethod(s, toUpperCase, []));

		var toString = s.field("toString");
		eq(s.toString(), Reflect.callMethod(s, toString, []));
	}
}
