package cases.display.issues;

class Issue6405 extends DisplayTestCase {
	/**
		import haxe.macro.Expr;
		import haxe.macro.Context;
		using haxe.macro.ExprTools;

		class Macros {

			public static macro function makeTypeDef( {-2-}unique_identifier_6405{-3-} : Expr ) {
				var t = Context.getType({-1-}unique_identifier_6405{-5-}.{-4-}toString());
				return macro {};
			}

		}
	**/
	function test(_) {
		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(2, 3), locs[0].range);

		Assert.equals("Expr", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)}).item.type.args.path.typeName);

		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(4), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "expr";
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "toString";
			case _: false;
		});
	}
}
