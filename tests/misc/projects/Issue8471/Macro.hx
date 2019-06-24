import haxe.macro.Context;

class Macro {
	public static function init() {
		Context.warning("This warning will disappear", Context.currentPos());

		Context.onAfterTyping(afterTyping);
		Context.onAfterGenerate(afterGenerate);
	}

	static function afterTyping(_) {
		var nbWarnings = Context.getWarnings().length;
		Context.clearWarnings();
		Context.warning('There were $nbWarnings warning(s) on after typing', Context.currentPos());
		Context.warning("This warning will not disappear", Context.currentPos());
		Context.warning("This warning will disappear too", Context.currentPos());
	}

	static function afterGenerate() {
		var nbWarnings = Context.getWarnings().length;
		Context.warning('There were $nbWarnings warning(s) on after generate', Context.currentPos());
		Context.filterWarnings(w -> w != "Warning : This warning will disappear too");
	}
}
