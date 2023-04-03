using StringTools;

function isLua5_1() {
	final proc = new sys.io.Process("lua", ["-v"]);
	final out = proc.stderr.readLine();
	proc.close();
	return out.startsWith("Lua 5.1.");
}

function matchesExpectedMessage(actual:String) {
	// lua 5.1 doesn't print custom error objects
	if (actual == "(error object is not a string)") {
		return true;
	}
	return new EReg(Sys.args()[1], "").match(actual);
}

function main() {
	final proc = new sys.io.Process("lua", [Sys.args()[0]]);

	// ignore "lua: "
	final exceptionMessage = proc.stderr.readLine().substr(5);

	final hasExpectedMessage = matchesExpectedMessage(exceptionMessage);
	// we don't want a bare error without a stack trace
	final hasStackTrace = try {
		proc.stderr.readLine().contains('stack traceback');
	} catch (_:haxe.io.Eof) {
		false;
	};

	Sys.println('Error code: ${proc.exitCode()}');
	Sys.println('Expected exception message: ${hasExpectedMessage}');
	// 5.1 interpreter doesn't handle custom objects
	Sys.println('Has call stack: ${hasStackTrace || isLua5_1()}');

	proc.close();
}
