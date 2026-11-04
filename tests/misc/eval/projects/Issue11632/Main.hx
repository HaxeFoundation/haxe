#if macro
import haxe.macro.Expr;
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
