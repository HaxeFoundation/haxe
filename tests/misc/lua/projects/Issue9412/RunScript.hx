using StringTools;

function noBuiltinBit() {
	final proc = new sys.io.Process("lua", ["-v"]);
	proc.exitCode();
	final err = proc.stderr.readAll().toString();
	proc.close();
	// Lua 5.1 has no builtin bit32 and no native bit operators.
	// Lua 5.2/5.3 have builtin bit32, Lua 5.3+ have native operators.
	return err.startsWith("Lua 5.1.");
}

function main() {
	var noRequirePass = Sys.command("lua", ["-e", "require = nil", "bin/main.lua"]) == 0;

	if (noBuiltinBit()) {
		// lua 5.1 has no builtin bit module and no native operators,
		// so will fail without require
		noRequirePass = !noRequirePass;
	}

	if (!(noRequirePass)) {
		Sys.exit(1);
	}
}
