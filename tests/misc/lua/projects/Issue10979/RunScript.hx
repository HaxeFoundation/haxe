using StringTools;

function main() {
	final proc = new sys.io.Process("lua", [Sys.args()[0]]);

	// if there is a line number, replace it with _
	final exceptionMessage = ~/:(\d+):/.replace(proc.stderr.readLine().substr(5), ":_:");
	// we don't want a bare error without a stack trace
	final hasStackTrace = proc.stderr.readLine().contains('stack traceback');

	final code = proc.exitCode();

	Sys.println('Error code: ${code}');
	Sys.println('Exception message: ${exceptionMessage}');
	Sys.println('Has call stack: ${hasStackTrace}');

	proc.close();
}
