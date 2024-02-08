import Main1.OtherClass.*;

@:publicFields
@:build(Macro.build())
class OtherClass {
	static var foo = 123;
}

class Main1 {
	static function main() {
		trace(foo); // Unknown identifier : foo
	}
}
