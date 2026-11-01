package pack;

@:native("NewClass")
class OldClass {
	macro static function f2() {
		return macro null;
	}

	public macro static function f1() {
		trace(OldClass);
		return macro null;
	}
}
