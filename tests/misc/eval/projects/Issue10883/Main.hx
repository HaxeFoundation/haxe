class Main {
	static function main() {
		var foo = {"a": 1, "b": 1, "c": 1};
		test();
		trace($i{"wtf"});
	}

	static macro function test() {
		var options:Array<haxe.macro.Expr> = [
			for (s in ["a", "b", "c"])
				macro @:pos(pos) $p{["foo", s]}
		];
		return macro $b{options};
	}
}
