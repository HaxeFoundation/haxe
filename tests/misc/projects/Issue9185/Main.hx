class Main {
	static function main() {
		if(Type.resolveClass('CustomIterator') != null) {
			throw "CustomIterator should have been DCE'd";
		}
		if(Type.getInstanceFields(Main).indexOf('iterator') >= 0) {
			throw "Main.iterator should have been DCE'd";
		}
	}

	public function testUnification():Iterable<Int> {
		return this;
	}

	static public function testForLoop() {
		for(i in new CustomIterator()) {}
	}

	public function iterator():Iterator<Int> {
		return null;
	}
}

class CustomIterator {
	public inline function new() {

	}

	public inline function hasNext():Bool {
		return false;
	}

	public inline function next():Int {
		return 10;
	}
}