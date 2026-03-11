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
		var files = runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags:Array<Diagnostic<Any>> = (files != null && files.length > 0 && files[0].diagnostics != null) ? files[0].diagnostics : [];
		Assert.equals(0, diags.length);
	}
}
