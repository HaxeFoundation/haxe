#if macro
import haxe.macro.Compiler;

class Macro {
	public static function initMacro() {
		// `*` matches exactly one segment: only types directly in `pack`
		Compiler.addGlobalMetadata("pack.*", "@onestar", false, true, false);
		// `**` matches any depth: every type under `pack`
		Compiler.addGlobalMetadata("pack.**", "@twostar", false, true, false);
		// same, but for the `foo` field
		Compiler.addGlobalMetadata("pack.*.foo", "@fonestar", false, false, true);
		Compiler.addGlobalMetadata("pack.**.foo", "@ftwostar", false, false, true);
	}
}
#end
