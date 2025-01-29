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

		var haxeServer = @:privateAccess BaseDisplayTestContext.haxeServer;
		Vfs.removeDir('${Sys.getCwd()}/test/cases');
		runner.run();
		haxeServer.close();
	}
}
