class Main {
	static function main() {
		var a:Object = { foo: 12, bar: "13" };
		for (field in a) {
			trace (field);
		}
		var a:ForwardedObject = { foo: 12, bar: "13" };
		for (field in a) {
			trace (field);
		}
	}
}

abstract Object(Dynamic) from Dynamic to Dynamic {
	public inline function new () {
		this = { };
	}
}

@:forward
abstract ForwardedObject(Dynamic) from Dynamic to Dynamic {
	public inline function new () {
		this = { };
	}
}