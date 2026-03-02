package cases.display.issues;

import haxe.display.Diagnostic;

class Issue11173 extends DisplayTestCase {
	/**
		class Main {
			static final {-1-}field{-2-} = 0;
			static function main() {
				{-3-}field{-4-} = 5;

				final foo = new Foo();
				foo.{-5-}field{-6-} = "ho${-9-}la";
			}
		}
		class Foo {
			public final {-7-}field{-8-} = "hi";
			public function new() {}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.isTrue(diags.exists(d -> d.kind == DKCompilerError && Std.string(d.range) == Std.string(range(3, 4))
			&& (d.args:String).indexOf("writing") != -1));
		Assert.isTrue(diags.exists(d -> d.kind == DKCompilerError && Std.string(d.range) == Std.string(range(5, 6))
			&& (d.args:String).indexOf("writing") != -1));

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(4)});
		Assert.equals("Int", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(6)});
		Assert.equals("String", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(4)});
		Assert.same(range(1, 2), parseGotoDefintion().result[0].range);

		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(6)});
		Assert.same(range(7, 8), parseGotoDefintion().result[0].range);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(9)});
		Assert.equals("String", parseHover().result.item.type.args.path.typeName);
	}
}
