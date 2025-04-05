class Macro {
  public static function build() {
	return haxe.macro.Context.getBuildFields().concat((macro class A {
		function bar() var a = main();
	}).fields);
  }
}
