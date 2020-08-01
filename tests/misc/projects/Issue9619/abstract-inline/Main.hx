abstract class Abstract {
	abstract inline public function abstractFunction():Void;
}

class Main {
	static function main() {
		var w:Abstract = null;
		w.abstractFunction();
	}
}
