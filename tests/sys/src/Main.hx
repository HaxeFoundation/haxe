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
		runner.addCase(new io.TestProcess());
		runner.addCase(new db.TestSqliteConnection());
		runner.addCase(new db.TestSqliteResultSet());
		#if php
		switch (Sys.systemName()) {
			case "Windows":
				// pass
			case _:
				runner.addCase(new net.TestSocket());
		}
		#else
			runner.addCase(new net.TestSocket());
		#end
		var report = Report.create(runner);
		report.displayHeader = AlwaysShowHeader;
		report.displaySuccessResults = NeverShowSuccessResults;
		runner.run();
	}
}
