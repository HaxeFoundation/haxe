class Main {
	var foo:foo.Foo;

	public static function main() include();

	static macro function include() {
		haxe.macro.Compiler.include("bar");
		return macro null;
	}
}
