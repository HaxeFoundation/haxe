#if macro
import haxe.macro.Context;
#end
import haxe.macro.Expr;

class Macro {
	// a long body, slow to generate
	public static macro function long(arr:Expr, n:Int):Expr {
		var el = [macro var x = 0];
		for (i in 1...n)
			el.push(macro x = x * $v{i} + $arr[$v{i % 3}]);
		el.push(macro x);
		return macro $b{el};
	}

	#if macro
	// many fields, generated first
	public static function statics(n:Int):Array<Field> {
		var fields = Context.getBuildFields();
		for (i in 1...n)
			fields.push({
				name: "m" + i,
				access: [APublic, AStatic],
				pos: Context.currentPos(),
				kind: FFun({args: [{name: "x", type: macro :Int}], ret: macro :Int, expr: macro return x + $v{i}})
			});
		return fields;
	}
	#end
}
