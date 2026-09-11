package unit.issues;

private typedef Sheet = Array<Rule>;
private typedef Rule = { name : String, ?subRules : Sheet };

class Issue12608 extends Test {
	function test() {
		var all : Sheet = [];
		var r : Rule = { name : "a" };
		all.push(r);
		if( r.subRules == null ) r.subRules = [];
		r.subRules.push({ name : "b" });
		eq(1, all.length);
		eq(1, r.subRules.length);
	}
}
