package unit.issues;

import scripthost.Issue12376;

class Issue12376 extends Test {
	#if cppia
	public function test() {
		var child:ScriptChild = new ScriptChild();
		eq('HostChild.methodA()', child.methodA());
		eq('ScriptChild.methodB()', child.methodB());
		eq('HostChild.methodC()', child.methodC());
	}
	#end
}

#if cppia
private class ScriptChild extends HostChild12376 {
	override function methodB() {
		return 'ScriptChild.methodB()';
	}
}
#end
