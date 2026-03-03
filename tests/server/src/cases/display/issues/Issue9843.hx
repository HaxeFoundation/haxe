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
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.equals(0, diags.length);
	}
}
