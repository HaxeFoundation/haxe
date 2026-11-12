import pack.E;
import pack.Thing;

class Shadowed {
	public static function test() {
		trace(Thing);
		trace(Own);
	}
}

enum OwnEnum {
	Own;
}

class Own {}
