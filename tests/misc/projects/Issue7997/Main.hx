enum Dummy {
	One;
	Two;
	Three;
}

class Main {
	static function main() {}

	function guessName<T>():Null<String> {
		function loop(type) {
			return switch (type.kind) {
				case One: type.args.path.typeName;
				case Two: loop(type.args);
				case _:
					$type(type);
					null;
			}
		}
		return loop(null);
	}
}