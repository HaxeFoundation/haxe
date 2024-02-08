macro function test() {
	trace(Sys.args());
	return macro null;
}

function main() {
	test();
	trace(Sys.args());
}
