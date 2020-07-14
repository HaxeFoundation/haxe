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
class IntermissionAbstract extends Abstract {
	@:overload
	override function abstractFunction():Void {}
}

class Main extends IntermissionAbstract {
	static function main() {

	}
}
