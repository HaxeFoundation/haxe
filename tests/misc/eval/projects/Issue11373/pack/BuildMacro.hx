package pack;

import haxe.macro.Context;

class BuildMacro {
	#if macro
	// read by another expansion: macro statics are reset for every expansion
	@:persistent static var builtTypes:Array<String> = [];
	#end

	static public function build():Array<haxe.macro.Expr.Field> {
		#if macro
		builtTypes.push("" + Context.getLocalClass());
		#end
		return null;
	}

	macro static public function report() {
		builtTypes.sort(Reflect.compare);
		return macro $v{builtTypes.join(", ")};
	}
}