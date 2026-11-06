import haxe.macro.Compiler;
import haxe.macro.Context;

class Report {
	static final tracked = ["haxe_ds_BalancedTree", "LibUnused", "UserUnused"];

	static function use() {
		Context.onAfterGenerate(() -> {
			final output = sys.io.File.getContent(Compiler.getOutput());
			sys.FileSystem.deleteFile(Compiler.getOutput());
			final kept = [for (name in tracked) if (output.indexOf('$name = ') != -1) name];
			Context.info("Kept: " + (kept.length == 0 ? "none" : kept.join(", ")), Context.currentPos());
		});
	}
}
