import haxe.macro.Expr;

class Main {
	static function main() {
		print("Hello");

		var val:MyString = "World";
		print(val);
	}

	static function print(msg:Message) {
		Sys.println(msg);
	}
}

abstract MyString(String) from String to String {}

abstract Message(Dynamic) from Dynamic {
	@:from macro static function from<T>(value:ExprOf<T>):ExprOf<Message> {
		return macro Std.string($value);
	}
}