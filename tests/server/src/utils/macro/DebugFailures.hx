package utils.macro;

import haxe.macro.Context;
import haxe.macro.Expr;

/**
 * Temporary. To find out what's wrong with random CI failures
 */
class DebugFailures {
	macro static public function patchAssert():Array<Field> {
		var fields = Context.getBuildFields();
		for(field in fields) {
			if(field.name == 'processResult') {
				switch field.kind {
					case FFun(fn):
						fn.expr = macro {
							if(!cond) js.Browser.console.log(pos.fileName + ":" + pos.lineNumber + ": " + haxe.Json.stringify(TestCase.debugLastResult));
							${fn.expr};
						}
					case _:
				}
			}
		}
		return fields;
	}
}