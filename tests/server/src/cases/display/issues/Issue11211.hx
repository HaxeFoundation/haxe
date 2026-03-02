package cases.display.issues;

import haxe.display.Diagnostic;

class Issue11211 extends DisplayTestCase {
	/**
		import haxe.macro.Context;
		import haxe.macro.Expr;
		using haxe.macro.Tools;

		class SafeAst {
			macro public static function build():Array<Field> {
				final fields = Context.getBuildFields();
				for (field in fields) {
					var expr:Null<Expr> = switch (field.kind) {
						case FVar(_, e): e;
						case FProp(_, _, _, e): e;
						case FFun(fn): fn.expr;
					}
					if (expr != null) {
						expr.expr = transform(expr).expr;
					}
				}
				return fields;
			}

			static function transform(expr:Expr):Expr {
				return expr.map(transform);
			}
		}

		#if !macro
		@:build(Main.SafeAst.build())
		class Main {
			static function main() {
				var errRa{-1-}nge = 0;
				{-4-}final pre{-2-}vId = trace("arg");{-5-}
				if (errRange != null) {
					final has{-3-}Comma = false;
				}
			}
		}
		#end
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("Int", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.equals("Void", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)});
		Assert.equals("Bool", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		var diag = diags.find(d -> d.kind == DKCompilerError && (d.args:String).indexOf("Void") != -1);
		Assert.notNull(diag);
		Assert.same(range(4, 5), diag.range);
	}
}
