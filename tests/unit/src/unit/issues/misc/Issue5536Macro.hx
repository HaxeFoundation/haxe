package unit.issues.misc;

import haxe.macro.Expr;

class Issue5536Macro {
	static var built = [];

	static macro function build():Array<Field> {
		built.push(haxe.macro.Context.getLocalClass().toString());
		return null;
	}

	static public macro function getBuilt() {
		built.sort(Reflect.compare);
		return macro $v{built.join(", ")};
	}
}
