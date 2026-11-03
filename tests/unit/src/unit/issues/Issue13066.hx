package unit.issues;

import scripthost.Issue13066;

class Issue13066 extends Test {
	#if cpp
	public function test() {
		// Std.string does not call a toString with arguments: it falls back on the class name,
		// without its package in native code and with it in cppia
		var child = new ScriptChild();
		t(StringTools.endsWith(Std.string(child), 'ScriptChild'));
		eq('HostParent.toString(1)', child.toString(1));

		var over = new ScriptOverride();
		t(StringTools.endsWith(Std.string(over), 'ScriptOverride'));
		eq('HostOverride.toString(1)', over.toString(1));

		var staticChild = new ScriptStaticChild();
		t(StringTools.endsWith(Std.string(staticChild), 'ScriptStaticChild'));
		eq('HostStaticParent.toString(1)', HostStaticParent13066.toString(1));
	}
	#end
}

#if cpp
private class ScriptChild extends HostChild13066 {}

private class ScriptOverride extends HostOverride13066 {}

private class ScriptStaticChild extends HostStaticChild13066 {}
#end
