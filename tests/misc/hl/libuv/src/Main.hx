import utest.ui.Report;
import utest.Runner;

function main() {
	var runner = new Runner();
	var report = Report.create(runner);
	report.displayHeader = AlwaysShowHeader;
	report.displaySuccessResults = NeverShowSuccessResults;
	runner.addCases('cases');
	runner.run();
}