package cases.display.issues;

class Issue7877 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				new issue7877.ProcessedClass(false);
				new issue7877.ProcessedClass(true);
			}
		}
	**/
	function test(_) {
		vfs.putContent("issue7877/ProcessedClass.hx", getTemplate("display/issues/Issue7877/ProcessedClass.hx"));
		vfs.putContent("issue7877/ProcessMacro.hx", getTemplate("display/issues/Issue7877/ProcessMacro.hx"));
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.equals(0, diags.length);
	}
}
