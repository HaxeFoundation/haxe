import haxe.macro.Expr;
import haxe.macro.Context;

class Main {
	static function main() {
		invalidPackage();
	}

	static macro function invalidPackage() {
		return {
			expr: ENew({name: "Foo", pack: ["0"]}, []),
			pos: Context.currentPos()
		};
	}
}
