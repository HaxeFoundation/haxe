package unit.issues;

using unit.issues.Issue4397;

class Issue4397 extends Test {
	function test() {
		0.toFixed();
	}

    static inline function toFixed(f:Float):String {
        return (cast f).toFixed();
    }
}