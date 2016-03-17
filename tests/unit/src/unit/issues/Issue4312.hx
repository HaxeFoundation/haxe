package unit.issues;

class Issue4312 extends Test {
   function test():Void {
		var a = getValue();
		if (!a) return null;
    }

	static function getValue() {
		return false;
	}
}