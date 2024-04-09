#if macro
import haxe.macro.Expr;
import haxe.macro.Type;
#end

#if !macro @:build(Macro.build()) #end
class Main {}

class Macro {
	public static function build() {
		var ct = TPath({pack: [], name: "Null", params: [TPType(null)]});
		return (macro class {
			var foo:$ct;
		}).fields;
	}
}
