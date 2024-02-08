import haxe.macro.Context;

class Macro {
	public static function init() {
		Context.onGenerate(function(_) {
			Context.filterMessages(msg -> switch msg {
				case Warning("haxe.Utf8 is deprecated. Use UnicodeString instead.", _): false;
				case _: true;
			});
		});
	}
}
