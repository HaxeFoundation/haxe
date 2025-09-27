package unit.issues;

import scripthost.Issue12374;

class Issue12374 extends Test {
	#if cppia
	public function test() {
		var child:ScriptChild = new ScriptChild();
		eq(Std.string(child), 'HostParent.toString()');
		eq(child.methodA(), 'ScriptChild.methodA()');
		eq(child.methodB(), 'HostParent.methodB()');
	}
	#end
}

#if cppia
private class ScriptChild extends HostParent {
	public override function methodA() {
		return 'ScriptChild.methodA()';
	}
}
#end
