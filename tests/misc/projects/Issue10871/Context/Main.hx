package;

#if macro
import haxe.macro.Context;
import haxe.macro.Type;
#end

function main() {
}

#if macro
class MacroClass {
	public static function start() {
		printTypedExpr(Context.getMainExpr());

		Context.onAfterTyping(function(modules: Array<ModuleType>) {
			printTypedExpr(Context.getMainExpr());
		});

		Context.onGenerate(function(types: Array<Type>) {
			printTypedExpr(Context.getMainExpr());

			if(Context.getAllModuleTypes().length == types.length) {
				trace("Module type list length == types.length");
			} else {
				trace("Module types length != types.length");
			}
		});
		
		Context.onAfterGenerate(function() {
			printTypedExpr(Context.getMainExpr());
		});
	}

	static function printTypedExpr(te: Null<TypedExpr>, ?pos:haxe.PosInfos) {
		if(te == null) {
			haxe.Log.trace("null", pos);
		} else {
			haxe.Log.trace(haxe.macro.TypedExprTools.toString(te, true), pos);
		}
	}
}
#end
