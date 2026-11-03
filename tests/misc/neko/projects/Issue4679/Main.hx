extern class C {
	static var PROXIMITY_BEGIN(default, never):String;
	static var PROXIMITY_END(default, never):String;
	static var PROXIMITY_MOVE(default, never):String;
}

@:native("C")
@:keep
class NotC {
	static var PROXIMITY_BEGIN = "PROXIMITY_BEGIN";
	static var PROXIMITY_END = "PROXIMITY_END";
	static var PROXIMITY_MOVE = "PROXIMITY_MOVE";
}

class Main {
	static function main() {
		function print(s:Dynamic) {
			Sys.stderr().writeString(s + "\n");
		}
		print(match(C.PROXIMITY_BEGIN));
		print(match(C.PROXIMITY_END));
		print(match(C.PROXIMITY_MOVE));
	}

	static function match(s:String) {
		return switch (s) {
			case C.PROXIMITY_BEGIN: 1;
			case C.PROXIMITY_END: 2;
			case _: 3;
		}
	}
}