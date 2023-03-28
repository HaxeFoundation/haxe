#if macro
import haxe.macro.Context;

function init() {
	var here = (macro here).pos;

	Context.onAfterInitMacros(() -> {
		Context.warning("1", here);
		Context.onAfterInitMacros(() -> {
			Context.warning("2", here);
		});
	});

	Context.onGenerate((_) -> {
		Context.warning("3", here);
		Context.onGenerate((_) -> {
			Context.warning("4", here);
		});
	});

	Context.onAfterGenerate(() -> {
		Context.warning("5", here);
		Context.onAfterGenerate(() -> {
			Context.warning("6", here);
		});
	});
}
#end

function main() {}
