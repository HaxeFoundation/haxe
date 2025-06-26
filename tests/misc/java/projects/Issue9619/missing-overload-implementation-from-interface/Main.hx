interface Interface {
	@:overload function toBeImplemented():Void;
	@:overload public function toBeImplemented(i:Int):Void;
}

@:keep
abstract class Abstract implements Interface {}

class NotAbstract extends Abstract {}

class Main {
	static function main() {

	}
}
