package unit.issues;

class Issue6276 extends unit.Test {
	function test(){
#if (!php7 && !java && !cs)
		var s = "foo";
		var indexOf = Reflect.field(s, "indexOf");
		var pos = Reflect.callMethod(s, indexOf, ["o"]);
		eq(pos, 1);
#end
	}
}
