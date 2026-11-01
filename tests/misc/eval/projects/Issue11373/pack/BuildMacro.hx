package pack;

import haxe.macro.Context;

class BuildMacro {
	#if macro
	static var builtTypes = [];
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