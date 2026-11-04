class Outcome {
	static function fromEither(ei:haxe.ds.Option<Err>) {
		switch ei {
			case Some(o):
			case _:
		}
	}
}

abstract Err(Bool) {
	@:from static public function fromInt(n:Int) {
		return new Err(here);
	}
}

class Main {
	static function main() {
		var err:Err = 5;
		trace(err);
	}
}