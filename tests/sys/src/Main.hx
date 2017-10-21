import utest.Runner;
import utest.ui.Report;

class Main {
	static public function main() {
		var runner = new Runner();
		runner.addCase(new TestSys());
		runner.addCase(new TestFileSystem());
		runner.addCase(new io.TestFile());
		runner.addCase(new io.TestFileInput());
		runner.addCase(new io.TestProcess());
		runner.addCase(new net.TestSocket());
		#if !(python || lua)
		runner.addCase(new db.TestSqlite());
		#end
		Report.create(runner);
		runner.run();
	}
}