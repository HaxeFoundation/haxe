import haxe.macro.Compiler;
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Expr.Field;
class Main {
    static function main() OldClass.define();
}

#if !macro
@:build(OldClass.build())
#end
@:native("NewClass")
class OldClass {
	#if !macro
	public function new() {};
	#end

	macro public static function define():Expr return macro new OldClass();

	macro static function build():Array<Field>
	{
		var defined = false;

		Context.onAfterTyping(_ -> {
			if (defined) return;

			Context.defineType(
				macro class NewClass {
					public function new() {}
				}
			);

			Compiler.exclude('OldClass');

			defined = true;
		});

		return null;
	}
}