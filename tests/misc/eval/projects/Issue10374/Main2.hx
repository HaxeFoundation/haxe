class Main2 {
	static function main() {
		// Works fine if Test wasn't loaded
		// Note that types/fields declared in Test.hx will become unusable
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
