abstract
class Abstract {
	@:abstract
	@:overload
	function abstractFunction():Void;

	@:abstract
	@:overload
	function abstractFunction(i:Int):Void;
}

abstract
class IntermissionAbstract extends Abstract {}

class Main extends IntermissionAbstract {
	static function main() {

	}

	@:overload
	override function abstractFunction():Void {}
}
