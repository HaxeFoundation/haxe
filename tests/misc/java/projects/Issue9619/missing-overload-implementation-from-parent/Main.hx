abstract
class Abstract {
	@:overload
	abstract
	function abstractFunction():Void;

	@:overload
	abstract
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
