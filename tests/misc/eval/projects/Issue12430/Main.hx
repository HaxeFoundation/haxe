class Main {
	static function main() {}

	static function init() {
		#if macro
		haxe.macro.Compiler.registerCustomDefine({
			define: "this_is_a_define",
			doc: "This is documentation related to the define",
			platforms: [Cpp, Jvm]
		}, null);
		haxe.macro.Compiler.registerCustomMetadata({
			metadata: "this_is_metadata",
			doc: "This is documentation related to the metadata",
			platforms: [Eval, Cross]
		});
		#end
	}
}
