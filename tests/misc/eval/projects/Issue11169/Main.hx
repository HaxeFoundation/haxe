function main() {
	var path = haxe.io.Path.normalize(Sys.programPath());
	if (haxe.io.Path.withoutDirectory(path) != "Main.hx" || !haxe.io.Path.isAbsolute(path)) {
		throw 'Unexpected program path: $path';
	}
}
