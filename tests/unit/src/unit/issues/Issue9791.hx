package unit.issues;

#if jvm

overload function moduleOverload(i:Int) {
	return "Int: " + i;
}

overload function moduleOverload(s:String) {
	return "String: " + s;
}

#end

class Issue9791 extends unit.Test {
	#if jvm
	function test() {
		eq("Int: 12", moduleOverload(12));
		eq("String: foo", moduleOverload("foo"));
	}
	#end
}
