class Main extends MyAbstractClass {
	static function main() {
		trace('myVar = ${new Main().myVar}');
	}

	public function new() {}
}

abstract class MyAbstractClass implements MyInterface {
	public final myVar:Int;
}

@:autoBuild(Macro.build())
interface MyInterface {
	public final myVar:Int;
}
