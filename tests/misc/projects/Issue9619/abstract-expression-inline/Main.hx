abstract class Abstract {
	abstract public function abstractFunction():Void;
}

class Main {
	static function main() {
		var w:Abstract = null;
		inline w.abstractFunction();
	}
}
