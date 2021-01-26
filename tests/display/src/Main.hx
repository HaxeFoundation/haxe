import utest.Runner;
import utest.ui.Report;

class Main {
	static function main() {
		var runner = new Runner();
		for (c in Macro.getCases("cases")) {
			runner.addCase(c);
		}
		var report = Report.create(runner);
		report.displayHeader = AlwaysShowHeader;
		report.displaySuccessResults = NeverShowSuccessResults;

		var haxeServer = @:privateAccess DisplayTestContext.haxeServer;
		haxeServer.setDefaultRequestArguments(["-cp", "src", "-cp", "src-shared", "--no-output", "-lib", "utest"]);
		DisplayTestContext.runHaxe([]);
		runner.run();
		haxeServer.close();
	}
}
