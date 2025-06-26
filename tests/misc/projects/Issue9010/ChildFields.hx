class ChildFields extends Parent {
	static function main() {}

	public function noOverride():String {
		return null;
	}
	override public function inlined():String {
		return null;
	}
}

class Parent {
	public function noOverride():String {
		return null;
	}
	public inline function inlined():String {
		return null;
	}
}