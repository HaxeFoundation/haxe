package cases.display.issues;

class Issue8066 extends DisplayTestCase {
	/**
		#if macro
		import haxe.macro.Context;
		import haxe.macro.Expr.Field;

		class Macro {
			static function build():Array<Field> {
				var fields = Context.getBuildFields();
				fields.{-1-}
				return fields;
			}
		}
		#end
	**/
	function testIfMacro(_) {
		vfs.putContent("Macro.hx", markers.source);
		vfs.putContent("Main.hx", "@:build(Macro.build()) class Main { static function main() {} }");
		checkCompletion("Macro.hx");
	}

	/**
		import haxe.macro.Context;
		import haxe.macro.Expr.Field;

		class Macro {
			static function build():Array<Field> {
				var fields = Context.getBuildFields();
				fields.{-1-}
				return fields;
			}
		}
	**/
	function testMacroHx(_) {
		vfs.putContent("Macro.macro.hx", markers.source);
		vfs.putContent("Macro.hx", "class Macro {}");
		vfs.putContent("Main.hx", "@:build(Macro.build()) class Main { static function main() {} }");
		checkCompletion("Macro.macro.hx");
	}

	@:coroutine
	function checkCompletion(file:String) {
		var result = runHaxeJson(["--main", "Main", "--js", "main.js"], DisplayMethods.Completion, {
			file: new FsPath(file),
			offset: offset(1),
			wasAutoTriggered: false
		});
		Assert.equals(result.mode.kind, Field);
		Assert.isTrue(hasField(result.items, "concat"));
		Assert.isTrue(hasField(result.items, "push"));
	}
}
