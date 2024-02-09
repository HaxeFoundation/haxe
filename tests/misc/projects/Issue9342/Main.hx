class Main {
	static function main() {
		Macro.foo();
	}
}

@:genericBuild(Macro.buildFoo())
class Foo {}