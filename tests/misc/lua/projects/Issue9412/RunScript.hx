using StringTools;

function noBuiltinBit() {
	final proc = new sys.io.Process("lua", ["-v"]);
	proc.exitCode();
	final err = proc.stderr.readAll().toString();
	final out = proc.stdout.readAll().toString();
	proc.close();
	return err.startsWith("Lua 5.1.") || out.startsWith("Lua 5.4.");
}

function main() {
	// emulate no require, rely on builtin module
	var noRequirePass = Sys.command("lua", ["-e", "require = nil", "bin/main.lua"]) == 0;

	if (noBuiltinBit()) {
		// lua 5.1 and 5.4+ don't have a builtin module, so will fail without require.
		// ignore these failures for now
		noRequirePass = !noRequirePass;
	}

	// emulate no builtin bit32/bit, needs require
	final noBuiltinPass = Sys.command("lua", ["-e", "bit32, bit = nil, nil", "bin/main.lua"]) == 0;

	if (!(noRequirePass && noBuiltinPass)) {
		Sys.exit(1);
	}
}
