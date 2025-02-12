import haxe.macro.CompilationServer;
import haxe.macro.Context;

function hookInvalidateError() {
	Context.onAfterTyping((_) -> {
		CompilationServer.invalidateModule("Empty");
	});
}

function hookInvalidateCatch() {
	Context.onAfterTyping((_) -> {
		try {
			CompilationServer.invalidateModule("Empty");
		} catch (e:Dynamic) {
			Sys.println(Std.string(e));
		}
	});
}
