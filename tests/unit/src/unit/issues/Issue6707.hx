package unit.issues;

private typedef T1 = {
	var ?x:Int;
	var y:String;
}

private typedef T2 = {
	final ?x:Int;
	var y:String;
}

class Issue6707 extends unit.Test {
	function test() {
		var t1:T1 = { y: "foo" };
		var t2:T2 = { y: "foo" };
	}
}