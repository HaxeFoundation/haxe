package unit.issues;

#if java
@:keep
private class NotMain implements java.util.Iterator<String> {
	static function main() {}

	public function hasNext():Bool
		throw 0;

	public function next():String
		throw 0;
}
#end

class Issue10618 extends unit.Test {
	public function test() {
		utest.Assert.pass();
	}
}
