package unit.issues;

import scripthost.Issue12413;

class Issue12413 extends Test {
	#if cppia
	public function test() {
		var child:ScriptChild = new ScriptChild();
		eq('HostParent, HostChild, ScriptChild', child.methodA());
	}
	#end
}

#if cppia
private class ScriptChild extends HostChild12413 {
	override function methodA() {
		return super.methodA() + ", ScriptChild";
	}
}
#end
