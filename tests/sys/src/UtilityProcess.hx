/**
	Used by TestUnicode.
	Runs a given simple program based on the first argument.
 */
class UtilityProcess {
	public static function main():Void {
		var args = Sys.args();
		switch (args) {
			case _.slice(0, 1) => ["putEnv"]:
			Sys.putEnv(args[1], args[2]);
			Sys.exit(Sys.command(args[3], args.slice(4)));
			case ["getCwd"]: Sys.println(Sys.getCwd());
			case ["getEnv", name]: Sys.println(Sys.getEnv(name));
			case ["exitCode", Std.parseInt(_) => code]: Sys.exit(code);
			case ["println", data]: Sys.println(data);
			case ["print", data]: Sys.print(data);
			case ["stdin.readLine"]: Sys.println(Sys.stdin().readLine());
			case ["stdin.readString", Std.parseInt(_) => len]: Sys.println(Sys.stdin().readString(len, UTF8));
			case ["stdin.readUntil", Std.parseInt(_) => end]: Sys.println(Sys.stdin().readUntil(end));
			case ["stderr.writeString", data]: var stream = Sys.stderr(); stream.writeString(data); stream.flush();
			case ["stdout.writeString", data]: var stream = Sys.stdout(); stream.writeString(data); stream.flush();
			case ["programPath"]: Sys.println(Sys.programPath());
			case _: // no-op
		}
	}
}
