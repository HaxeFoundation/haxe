/**
	Used by TestUnicode.
	Runs a given simple program based on the first argument.
 */

class UtilityProcess {
	public static function main():Void {
		var args = Sys.args();
		function sequenceIndex(d:Int, returnNFC:Bool):String return (switch (UnicodeSequences.valid[d]) {
				case Only(ref): UnicodeSequences.codepointsToString(ref);
				case Normal(nfc, nfd): UnicodeSequences.codepointsToString(returnNFC ? nfc : nfd);
			});
		switch (args) {
			case _.slice(0, 1) => ["putEnv"]:
			Sys.putEnv(args[1], args[2]);
			Sys.exit(Sys.command(args[3], args.slice(4)));
			case ["getCwd"]: Sys.println(Sys.getCwd());
			case ["getEnv", name]: Sys.println(Sys.getEnv(name));
			case ["exitCode", Std.parseInt(_) => code]: Sys.exit(code);
			case ["args", data]: Sys.println(data);
			case ["println", Std.parseInt(_) => d, mode]: Sys.println(sequenceIndex(d, mode == "nfc"));
			case ["print", Std.parseInt(_) => d, mode]: Sys.print(sequenceIndex(d, mode == "nfc"));
			case ["stdin.readLine"]: Sys.println(Sys.stdin().readLine());
			case ["stdin.readString", Std.parseInt(_) => len]: Sys.println(Sys.stdin().readString(len, UTF8));
			case ["stdin.readUntil", Std.parseInt(_) => end]: Sys.println(Sys.stdin().readUntil(end));
			case ["stderr.writeString", Std.parseInt(_) => d, mode]:
			var stream = Sys.stderr(); stream.writeString(sequenceIndex(d, mode == "nfc")); stream.flush();
			case ["stdout.writeString", Std.parseInt(_) => d, mode]:
			var stream = Sys.stdout(); stream.writeString(sequenceIndex(d, mode == "nfc")); stream.flush();
			case ["programPath"]: Sys.println(Sys.programPath());
			case _: // no-op
		}
	}
}
