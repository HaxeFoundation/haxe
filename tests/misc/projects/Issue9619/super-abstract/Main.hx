abstract class Abstract {
	abstract function abstractFunction():Void;
}

class Main extends Abstract {
	static function main() {

	}

	function abstractFunction() {
		super.abstractFunction();
	}
}
