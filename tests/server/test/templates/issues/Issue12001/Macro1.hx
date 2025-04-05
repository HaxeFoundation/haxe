import haxe.macro.CompilationServer;
import haxe.macro.Context;
import haxe.macro.Expr.Error;

function hookInvalidateError() {
	Context.onAfterTyping((_) -> {
		CompilationServer.invalidateModule("Empty");
	});
}

function hookInvalidateCatch() {
	Context.onAfterTyping((_) -> {
		try {
			CompilationServer.invalidateModule("Empty");
		} catch (e:Error) {
			Sys.println(e.message);
		}
	});
}
