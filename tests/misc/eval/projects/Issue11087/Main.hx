macro function test() {
	trace(filterArgs(Sys.args()));
	return macro null;
}

function main() {
	test();
	trace(filterArgs(Sys.args()));
}

function filterArgs(args:Array<String>):Array<String> {
	if (args.length < 2) return args;
	// We're currently prepending that to all tests while moving to pretty errors by default
	if (args[0] == "-D" && args[1] == "message.reporting=classic") return args.slice(2);
	return args;
}
