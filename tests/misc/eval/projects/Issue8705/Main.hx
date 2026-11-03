class Main {
	public static function main():Void {
		testMacro()();
	}

	static macro function testMacro() {
		return {
			expr: EFunction(null, {args: [], expr: macro {}, ret: null}),
			pos: haxe.macro.Context.currentPos()
		};
	}
}
