import utest.ui.Report;
import utest.Runner;
import utils.Vfs;

class Main {
	static public function main() {
		Vfs.removeDir("test/cases");

		var runner = new Runner();
		runner.addCases('cases');
		var report = Report.create(runner);
		report.displayHeader = AlwaysShowHeader;
		report.displaySuccessResults = NeverShowSuccessResults;
		runner.run();
	}
}