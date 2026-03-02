package cases.display.issues;

import haxe.display.Diagnostic;

class Issue7777 extends DisplayTestCase {
	/**
		{-1-}import issue7777.Thing;{-2-}
		import issue7777.Foo;

		class Main {
			public static function main() {
				var foo:Foo<String> = BOO;
				trace(foo);
			}
		}

	**/
	function test(_) {
		vfs.putContent("issue7777/Thing.hx", "package issue7777;\n\nenum Thing {\n\tBOO;\n}");
		vfs.putContent("issue7777/Foo.hx", "package issue7777;\n\n#if (eval || macro)\nimport haxe.macro.Expr;\n#end\n\nabstract Foo<T>(T) {\n\t@:from public static macro function fromThing<T>(e:ExprOf<Thing>):ExprOf<Foo<T>> {\n\t\treturn macro null;\n\t}\n}");
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKUnusedImport);
		Assert.equals(Warning, diags[0].severity);
		Assert.same(range(1, 2), diags[0].range);
	}
}
