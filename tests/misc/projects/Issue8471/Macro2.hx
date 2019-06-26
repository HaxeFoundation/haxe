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
		var order = Lambda.fold(warnings, (w, acc) -> w.message.length == 1 ? acc + w.message.charAt(0) : acc, "");

		Context.filterWarnings(function(w, _) {
			if (w.length == 1) order += w.charAt(0);
			return true;
		});

		Context.warning(order, Context.currentPos());
	}
}
