import haxe.macro.Compiler;
import haxe.macro.Context;

function init() {
	if (Context.defined("haxe")) Context.warning("ok", (macro 0).pos);

	Context.onAfterInitMacros(() -> {
		Context.warning("after init 1", (macro 0).pos);
		var e = macro 42;
		Context.typeof(e);
	});
	Compiler.include("hax.ds", true, true);
	Context.onAfterInitMacros(() -> {
		Context.warning("after init 2", (macro 0).pos);
	});
}

function init_fail() {
	var e = macro 42;
	Context.typeof(e);
	Compiler.define("foo", "foo");

	Context.onAfterInitMacros(() -> {
		Compiler.define("bar", "bar");
	});
}
