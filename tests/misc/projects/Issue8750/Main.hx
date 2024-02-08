class Main {
	static function main () {
		#if !macro
		define();
		#end
	}

	macro static public function define() {
		haxe.macro.Context.defineType({
			pack:["hxd","_res"],
			name:"_Ui_skills",
			pos:haxe.macro.Context.currentPos(),
			kind:TDStructure,
			fields:[]
		});
		return macro {};
	}
}