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

		var messages = Context.getMessages();
		var order = Lambda.fold(messages, (msg, acc) -> switch msg {
			case Warning(w, _) if (w.length == 1): acc + w;
			case Info(_, _): acc + 'i';
			case _: acc;
		}, "") + "|";

		Context.filterMessages(function(msg) {
			switch msg {
				case Warning(w, _) if (w.length == 1): order += w;
				case Info(_, _): order += 'i';
				case _:
			}

			return true;
		});

		Context.warning(order, Context.currentPos());
	}
}
