class Main {
	static function main() {
		Test.foo();
		test();
	}

	static macro function test() {
		haxe.macro.Context.defineModule("Test", [{
			pos: (macro moduleField).pos,
			pack: [],
			name: 'moduleField',
			params: [],
			meta: [],
			kind: TDField(FVar(null, macro null), [AInline, AFinal]),
			fields: []
		}]);

		return macro {};
	}
}
