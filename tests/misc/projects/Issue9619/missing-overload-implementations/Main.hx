@:abstract
class Abstract {
	@:abstract
	@:overload
	function abstractFunction():Void;

	@:abstract
	@:overload
	function abstractFunction(i:Int):Void;
}

class Main extends Abstract {
	static function main() {

	}
}
