package utils.macro;

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;

class BuildHub {
	macro static public function build():Array<Field> {
		var fields = Context.getBuildFields();

		switch Context.getLocalClass() {
			case null:
			case _.get() => cls:
				if(isDisplayTest(cls)) {
					fields = DisplayTestBuilder.build(fields);
				}
		}

		return TestBuilder.build(fields);
	}

	static function isDisplayTest(cls:ClassType):Bool {
		if(cls.pack.length == 0 && cls.name == "DisplayTestCase") {
			return true;
		}
		return switch cls.superClass {
			case null: false;
			case _.t.get() => cls: isDisplayTest(cls);
		}
	}
}