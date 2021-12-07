import haxe.macro.Context;

function main() {
	// test that built in defines can be accessed by macros
	// either with a dash or underscore

	if (!Context.defined("haxe-ver"))
		throw "`haxe-ver` flag is missing";

	if (!Context.defined("haxe_ver"))
		throw "`haxe_ver` flag is missing";
}
