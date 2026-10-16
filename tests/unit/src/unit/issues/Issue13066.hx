package unit.issues;

import scripthost.Issue13066;

class Issue13066 extends Test {
	#if cppia
	public function test() {
		// As in native code, Std.string does not call a toString with arguments
		var child = new ScriptChild();
		eq(Type.getClassName(ScriptChild), Std.string(child));
		eq('HostParent.toString(1)', child.toString(1));

		var over = new ScriptOverride();
		eq(Type.getClassName(ScriptOverride), Std.string(over));
		eq('HostOverride.toString(1)', over.toString(1));
	}
	#end
}

#if cppia
private class ScriptChild extends HostChild13066 {}

private class ScriptOverride extends HostOverride13066 {}
#end
