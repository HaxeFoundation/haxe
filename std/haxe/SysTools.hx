package haxe;

import haxe.ds.ReadOnlyArray;

class SysTools {
	/**
		Character codes of the characters that will be escaped by `quoteWinArg(_, true)`.
	**/
	public static final winMetaCharacters:ReadOnlyArray<Int> = [
		" ".code, "(".code, ")".code, "%".code, "!".code, "^".code, "\"".code, "<".code, ">".code, "&".code, "|".code, "\n".code, "\r".code, ",".code, ";".code
	];

	/**
		Returns a String that can be used as a single command line argument
		on Unix.
		The input will be quoted, or escaped if necessary.
	**/
	public static function quoteUnixArg(argument:String):String {
		// Based on cpython's shlex.quote().
		// https://hg.python.org/cpython/file/a3f076d4f54f/Lib/shlex.py#l278

		if (argument == "")
			return "''";

		if (!~/[^a-zA-Z0-9_@%+=:,.\/-]/.match(argument))
			return argument;

		// use single quotes, and put single quotes into double quotes
		// the string $'b is then quoted as '$'"'"'b'
		return "'" + StringTools.replace(argument, "'", "'\"'\"'") + "'";
	}

	/**
		Returns a String that can be used as a single command line argument
		on Windows.
		The input will be quoted, or escaped if necessary, such that the output
		will be parsed as a single argument using the rule specified in
		http://msdn.microsoft.com/en-us/library/ms880421

		Examples:
		```haxe
		quoteWinArg("abc") == "abc";
		quoteWinArg("ab c") == '"ab c"';
		```
	**/
	public static function quoteWinArg(argument:String, escapeMetaCharacters:Bool):String {
		// If there is no space, tab, back-slash, or double-quotes, and it is not an empty string.
		if (!~/^(\/)?[^ \t\/\\"]+$/.match(argument)) {
			// Based on cpython's subprocess.list2cmdline().
			// https://hg.python.org/cpython/file/50741316dd3a/Lib/subprocess.py#l620

			var result = new StringBuf();
			var needquote = argument.indexOf(" ") != -1 || argument.indexOf("\t") != -1 || argument == "" || argument.indexOf("/") > 0;

			if (needquote)
				result.add('"');

			var bs_buf = new StringBuf();
			for (i in 0...argument.length) {
				switch (argument.charCodeAt(i)) {
					case "\\".code:
						// Don't know if we need to double yet.
						bs_buf.add("\\");
					case '"'.code:
						// Double backslashes.
						var bs = bs_buf.toString();
						result.add(bs);
						result.add(bs);
						bs_buf = new StringBuf();
						result.add('\\"');
					case var c:
						// Normal char
						if (bs_buf.length > 0) {
							result.add(bs_buf.toString());
							bs_buf = new StringBuf();
						}
						result.addChar(c);
				}
			}

			// Add remaining backslashes, if any.
			result.add(bs_buf.toString());

			if (needquote) {
				result.add(bs_buf.toString());
				result.add('"');
			}

			argument = result.toString();
		}

		if (escapeMetaCharacters) {
			var result = new StringBuf();
			for (i in 0...argument.length) {
				var c = argument.charCodeAt(i);
				if (winMetaCharacters.indexOf(c) >= 0) {
					result.addChar("^".code);
				}
				result.addChar(c);
			}
			return result.toString();
		} else {
			return argument;
		}
	}
}
