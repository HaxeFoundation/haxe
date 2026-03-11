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
		var files = runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		final diags:Array<haxe.display.Diagnostic<Any>> = files != null && files.length > 0 ? cast files[0].diagnostics : [];
		var writingErrors = diags.filter(d -> d.kind == DKCompilerError && (d.args:String).indexOf("writing") != -1);
		Assert.equals(2, writingErrors.length);
		var diag1 = writingErrors.find(d -> Std.string(d.range) == Std.string(range(3, 4)));
		Assert.notNull(diag1);
		Assert.same(range(3, 4), diag1.range);
		var diag2 = writingErrors.find(d -> Std.string(d.range) == Std.string(range(5, 6)));
		Assert.notNull(diag2);
		Assert.same(range(5, 6), diag2.range);

		Assert.equals("Int", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(4)}).item.type.args.path.typeName);

		Assert.equals("String", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(6)}).item.type.args.path.typeName);

		Assert.same(range(1, 2), runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(4)})[0].range);

		Assert.same(range(7, 8), runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(6)})[0].range);

		Assert.equals("String", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(9)}).item.type.args.path.typeName);
	}
}
