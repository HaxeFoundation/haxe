import haxe.macro.Context;

using haxe.macro.Tools;

class Macro {
	public static function build() {
		var fields = Context.getBuildFields();
		var keywords = [
			"auto", "bool", "break", "case", "char", "const", "continue", "default", "do", "double", "else", "enum", "extern", "float", "for", "goto",
			"if", "int", "long", "register", "return", "short", "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned",
			"void", "volatile", "while",
			// Values
			"NULL", "true", "false",
			// MS specific
			"asm", "dllimport2", "dllexport2", "naked2", "thread2",
			// reserved by HLC
			"t",
			// GCC
			"typeof",
			// C11
			"_Alignas", "_Alignof", "_Atomic", "_Bool", "_Complex", "_Generic", "_Imaginary", "_Noreturn", "_Static_assert", "_Thread_local", "_Pragma",
			"inline", "restrict", "_restrict"
		];

		var pos = Context.currentPos();

		for (k in keywords) {
			fields.push({
				pos: pos,
				name: "_test_" + k,
				meta: [{pos: pos, name: ":native", params: [macro $v{k}]}],
				kind: FVar(macro :String, null)
			});

			fields.push({
				pos: pos,
				name: "_test2_" + k,
				meta: [{pos: pos, name: ":native", params: [macro $v{'__' + k}]}],
				kind: FVar(macro :String, null)
			});
		}

		return fields;
	}
}
