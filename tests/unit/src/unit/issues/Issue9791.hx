package unit.issues;

#if false

overload function moduleOverload(i:Int) {
	return "Int: " + i;
}

overload function moduleOverload(s:String) {
	return "String: " + s;
}

#end

	#if false
class Issue9791 extends unit.Test {
	function test() {
		eq("Int: 12", moduleOverload(12));
		eq("String: foo", moduleOverload("foo"));
	}
	#end
}