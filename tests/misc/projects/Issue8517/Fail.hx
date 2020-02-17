class Main {
	public static function main():Void {
		Macro.notAssignable() = 2;
		Foo.bar() = 2;
		Foo.bar() += 2;
	}
}