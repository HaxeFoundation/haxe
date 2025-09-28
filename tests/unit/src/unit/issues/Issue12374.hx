package unit.issues;

import scripthost.Issue12374;

class Issue12374 extends Test {
	#if cppia
	public function test() {
		var child:ScriptChild = new ScriptChild();
		eq('HostParent.toString()', Std.string(child));
		eq('ScriptChild.methodA()', child.methodA());
		eq('HostParent.methodB()', child.methodB());
	}
	#end
}

#if cppia
private class ScriptChild extends HostParent12374 {
	override function methodA() {
		return 'ScriptChild.methodA()';
	}
}
#end
