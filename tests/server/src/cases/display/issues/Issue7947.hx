package cases.display.issues;

import haxe.display.Diagnostic;

class Issue7947 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				var either = haxe.ds.Either.Left(123);
				return switch either {
					case Left(_):
						trace('Some logic...');
						trace('Some logic...');
						trace('Some logic...');
						true;
					{-1-}case Right(_):{-2-}
				}
			}
		}
	**/
	function test1(_) {
		var files = runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		final diags:Array<haxe.display.Diagnostic<Any>> = files != null && files.length > 0 ? cast files[0].diagnostics : [];
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKCompilerError);
		Assert.equals(Error, diags[0].severity);
		Assert.equals("Void should be Bool", diags[0].args);
		Assert.same(range(1, 2), diags[0].range);
	}

	/**
		class Main {
			public static function main() {
				var either = haxe.ds.Either.Left(123);
				return switch either {
					case Right(_):
					case Left(_):
						trace('Some logic...');
						trace('Some logic...');
						trace('Some logic...');
						{-1-}true{-2-};
				}
			}
		}
	**/
	function test2(_) {
		var files = runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		final diags:Array<haxe.display.Diagnostic<Any>> = files != null && files.length > 0 ? cast files[0].diagnostics : [];
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKCompilerError);
		Assert.equals(Error, diags[0].severity);
		Assert.equals("Bool should be Void", diags[0].args);
		Assert.same(range(1, 2), diags[0].range);
	}
}
