class Main {
	static function main() {
		var tests = Macro.getCases("cases");
		var numTests = 0;
		var numFailures = 0;
		for (test in tests) {
			try {
				var result = test.run();
				numTests += result.numTests;
				numFailures += result.numFailures;
			   Sys.println('${result.testName}: ${result.numTests} tests, ${result.numFailures} failures');
			} catch(e:DisplayTestContext.HaxeInvocationException) {
				Sys.println("Error:      " + e.message);
				Sys.println("Field name: " + e.fieldName);
				Sys.println("Arguments:  " + e.arguments.join(" "));
				Sys.println("Source: " + e.source);
				numTests++;
				numFailures++;
			}
		}
		Sys.println('Finished with $numTests tests, $numFailures failures');
		Sys.exit(numFailures == 0 ? 0 : 1);
	}
}
