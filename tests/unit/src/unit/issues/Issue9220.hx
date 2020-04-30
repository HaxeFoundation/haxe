package unit.issues;

import unit.Test;

class Issue9220 extends Test {
	#if java
	public function test() {
		eq("12.200", java.NativeString.format(java.util.Locale.US, '%.3f', 12.2));
	}
	#end
}