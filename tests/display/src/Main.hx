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
		haxeServer.setDefaultRequestArguments(["-cp", "src", "--no-output", "-lib", "utest"]);
		var api = new haxeserver.sync.HaxeMethods(haxeServer);
		api.server.readClassPaths();
		DisplayTestContext.runHaxe([]);
		runner.run();
	}
}
