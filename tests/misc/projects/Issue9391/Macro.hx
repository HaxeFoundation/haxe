import haxe.macro.Context;
import haxe.macro.Expr;

class Macro {
	public static function build():ComplexType {
		var localClass = Context.getLocalClass().get();
		localClass.meta.remove(":genericBuild");
		return null;
	}
}