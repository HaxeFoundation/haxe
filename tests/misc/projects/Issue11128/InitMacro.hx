import haxe.macro.Compiler;
import haxe.macro.Context;
import haxe.macro.Type;

class InitMacro {
	static function setup() {
		switch (Compiler.getConfiguration().platform) {
			case CustomTarget("mylang"): {}
			case _: throw "this shouldnt happen.";
		}

		Context.onAfterTyping(check);
	}

	static function check(types:Array<ModuleType>) {
		for (m in types) {
			switch (m) {
				case TClassDecl(_.get() => c):
					for (f in c.fields.get()) f.expr();

				case _:
			}
		}
	}
}
