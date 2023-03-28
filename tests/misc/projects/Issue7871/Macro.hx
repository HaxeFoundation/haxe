import haxe.macro.Compiler;
import haxe.macro.Context;

function init() {
	var e = macro 42;
	Context.typeof(e);
	if (Context.defined("haxe")) Context.warning("ok", (macro 0).pos);
	Compiler.define("foo", "foo");

	Context.onAfterInitMacros(() -> {
		Context.warning("after init 1", (macro 0).pos);
		Compiler.define("bar", "bar");
		Context.typeof(e);
	});
	Compiler.include("hax.ds", true, true);
	Context.onAfterInitMacros(() -> {
		Context.warning("after init 2", (macro 0).pos);
	});
}
