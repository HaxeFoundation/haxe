import utest.ui.Report;
import utest.Runner;
import utils.Vfs;
import haxeserver.HaxeServerAsync;
import haxeserver.process.HaxeServerProcessNode;

class Main {
	static public function main() {
		Vfs.removeDir("test/cases");

		var runner = new Runner();
		runner.addCases('cases');
		var report = Report.create(runner);
		report.displayHeader = AlwaysShowHeader;
		report.displaySuccessResults = NeverShowSuccessResults;
		var cwd = Sys.getCwd();
		var server:HaxeServerAsync = null;
		runner.onComplete.add(_ -> server.stop());
		server = new HaxeServerAsync(() -> new HaxeServerProcessNode("haxe", ["-v"], {}, () -> {
			TestCase.server = server;
			TestCase.rootCwd = cwd;
			runner.run();
		}));
	}
}
