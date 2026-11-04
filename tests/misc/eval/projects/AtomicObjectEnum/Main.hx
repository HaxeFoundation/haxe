import haxe.atomic.AtomicObject;

enum Outcome {
	Succeeded;
	Failed(error:String);
}

class Main {
	static function main() {
		// Enum values are reference types on every target, so an enum with payloads
		// should be usable with AtomicObject like a class instance is.
		final outcome = new AtomicObject<Null<Outcome>>(null);
		final failed = Failed("boom");
		if (outcome.compareExchange(null, failed) != null) {
			throw "the first compareExchange should win";
		}
		if (outcome.compareExchange(null, Succeeded) != failed) {
			throw "the second compareExchange should lose";
		}
		switch (outcome.load()) {
			case Failed("boom"):
			case other:
				throw 'unexpected outcome $other';
		}
		Sys.println("ok");
	}
}
