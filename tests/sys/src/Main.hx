import utest.Runner;
import utest.ui.Report;
import utest.ui.common.HeaderDisplayMode;

class Main {
	static public function main() {
		var runner = new Runner();
		runner.addCase(new TestUnicode());
		runner.addCase(new TestSys());
		runner.addCase(new TestFileSystem());
		runner.addCase(new io.TestFile());
		runner.addCase(new io.TestFileInput());
		#if !js
		runner.addCase(new io.TestProcess());
		#end
		runner.addCase(new net.TestSocket());
		var report = Report.create(runner);
		report.displayHeader = AlwaysShowHeader;
		report.displaySuccessResults = NeverShowSuccessResults;
		runner.run();
	}
}
