package cases.display.issues;

class Issue7027 extends DisplayTestCase {
	/**
		import haxe.macro.Expr.ExprDef.{-1-}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case EnumField: item.args.field.name == "EBreak";
			case _: false;
		});
	}
}
