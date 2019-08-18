package unit.issues;

using unit.issues.Issue4397;

class Issue4397 extends Test {
	#if js
	function test() {
		0.toFixed();
		noAssert();
	}

    static inline function toFixed(f:Float):String {
        return (cast f).toFixed();
    }
	#end
}