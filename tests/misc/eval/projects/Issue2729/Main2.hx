import Main2.OtherClass.foo; // fails directly on import

@:publicFields
@:build(Macro.build())
class OtherClass {
	static var foo = 123;
}

class Main2 {
	static function main() {
		trace(foo); // Unknown identifier : foo
	}
}
