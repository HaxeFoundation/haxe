import haxe.macro.Context;

class Macro {
	public static function init() {
		Context.warning("This warning will disappear", Context.currentPos());

		Context.onAfterTyping(afterTyping);
		Context.onAfterGenerate(afterGenerate);
	}

	static function afterTyping(_) {
		var nbMessages = Context.getMessages().length;
		Context.filterMessages(_ -> false);
		Context.warning('There were $nbMessages messages on after typing', Context.currentPos());
		Context.warning("This warning will not disappear", Context.currentPos());
		Context.warning("This warning will disappear too", Context.currentPos());
	}

	static function afterGenerate() {
		var nbMessages = Context.getMessages().length;
		Context.warning('There were $nbMessages messages on after generate', Context.currentPos());
		Context.filterMessages(msg -> switch msg {
			case Warning(w, _): w != "This warning will disappear too";
			case Info(_, _): true;
		});
	}
}
