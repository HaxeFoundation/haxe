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
class IntermissionAbstract extends Abstract {
	@:overload
	override function abstractFunction():Void {}
}

class Main extends IntermissionAbstract {
	static function main() {

	}
}
