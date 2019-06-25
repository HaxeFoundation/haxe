import haxe.macro.Context;

@:deprecated
typedef DeprecatedType = String;

class Macro2 {
	public static function init() {
		Context.onAfterTyping(afterTyping);
	}

	static function afterTyping(_) {
		Context.warning(("1" :DeprecatedType), Context.currentPos());
		Context.warning("2", Context.currentPos());
		Context.warning("3", Context.currentPos());

		var warnings = Context.getWarnings();
		var order = Lambda.fold(warnings, (w, acc) -> w.length == 11 ? acc + w.charAt(10) : acc, "");
		Context.filterWarnings(function(w) {
			if (w.length == 11) order += w.charAt(10);
			return true;
		});

		Context.warning(order, Context.currentPos());
	}
}
