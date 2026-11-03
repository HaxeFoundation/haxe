package unit.issues;

private typedef Keywords = {
	var "class":String;
	var ?"inline":Int;
	final "break":Bool;
	var ?"function":Keywords;
}

private typedef Short = {?"default":Int, "public":String};

class Issue7722 extends Test {
	function test() {
		var o:Keywords = {"class": "a", "inline": 1, "break": true};
		eq("a", o."class");
		eq(1, o."inline");
		t(o."break");
		o."class" = "b";
		eq("b", o."class");
		eq("b", Reflect.field(o, "class"));
		o."function" = {"class": "nested", "break": false};
		eq("nested", o."function"."class");

		var s:Short = {"public": "p"};
		eq("p", s."public");
		eq(null, s."default");
		s."default" = 2;
		eq(2, Reflect.field(s, "default"));
	}

	function testUndeclaredQuotedKeysStayAllowed() {
		var o:{?a:Int} = {"cast": 1, a: 2};
		eq(2, o.a);
	}
}
