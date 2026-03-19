private class HaxeExtendsSwc extends ParentCtorWithDefaultStringArgument {}

class Main {
	static function main() {
		var s:String = new HaxeExtendsSwc().strField;
	}
}
