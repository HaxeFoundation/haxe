class Main2 {
	static function main () {
		#if !macro
		define();
		#end
	}

	macro static public function define() {
		haxe.macro.Context.defineModule("some.+", [{
			pos: (macro 0).pos,
			pack: ["some"],
			name: "+",
			kind: TDClass(),
			fields: []
		}]);
		return macro {};
	}
}