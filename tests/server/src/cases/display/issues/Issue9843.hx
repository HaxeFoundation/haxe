package cases.display.issues;

class Issue9843 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				test(() -> {});
			}

			static function test(fn:()->Void) {}
		}
	**/
	function test(_) {
		var files = runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags:Array<Diagnostic<Any>> = (files != null && files.length > 0 && files[0].diagnostics != null) ? files[0].diagnostics : [];
		Assert.equals(0, diags.length);
	}
}
