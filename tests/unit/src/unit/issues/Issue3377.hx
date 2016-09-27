package unit.issues;

class Issue3377 extends Test {
	function test() {
		function f1(a:Array<Dynamic>) {
			return switch (a) {
				case [["a"]]: "match";
				default: "no match";
			}
		}
		eq("match", f1([["a"]]));
		eq("no match", f1([["b"]]));
		eq("no match", f1([[]]));

		function f2(a:Array<Dynamic>) {
			return switch (a) {
				case [[]]: "match";
				default: "no match";
			}
		}
		eq("no match", f2([["a"]]));
		eq("no match", f2([["b"]]));
		eq("no match", f2([["a", "b"]]));
		eq("no match", f2([[12]]));
		eq("match", f2([[]]));
	}

	function testAtry() {
		var complexObject =
		{
			field1: 123,
			field2: 8.0,
			field3: "foo",
			field4: ([ "foo", null, [], "bar", ([ ([ [], 2, null, { a: 0, } ]:Array<Dynamic>), "baz", [], ]: Array<Dynamic>), { b: null } ]: Array<Dynamic>),
			field5: null,
		}

		var s = switch (complexObject)
		{
			case {
				field1: 123,
				field2: 8.0,
				field3: "foo",
				field4: [ "foo", null, (_:Array<Dynamic>) => [], "bar", (_:Array<Dynamic>) => [ (_:Array<Dynamic>) => [ (_:Array<Dynamic>) => [], 2, null, (_: { ?a:Int }) => { a: 0, } ], "baz", [], ], (_: { ?b:Dynamic }) => { b: null } ],
				field5: null,
			}: "Matched!";
			default: "Not matched!";
		}
		eq("Matched!", s);
	}
}