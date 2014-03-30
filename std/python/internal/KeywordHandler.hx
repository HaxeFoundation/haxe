package python.internal;


import python.lib.Types.Set;


class KeywordHandler {


	static var keywords:Set<String> = new Set(
    [
        "and",       "del",       "from",      "not",       "while",
        "as",        "elif",      "global",    "or",        "with",
        "assert",    "else",      "if",        "pass",      "yield",
        "break",     "except",    "import",    "print",     "float",
        "class",     "exec",      "in",        "raise",
        "continue",  "finally",   "is",        "return",
        "def",       "for",       "lambda",    "try",
        "None",      "list"
    ]);

	public static inline function handleKeywords(name:String)
    {
        if (keywords.has(name)) {
            return "_hx_" + name;
        }
        return name;
    }

    public static function unhandleKeywords(name:String)
    {
    	if (name.substr(0,4) == "_hx_") {
    		var real = name.substr(4);
    		if (keywords.has(real)) return real;
    	}
    	return name;
    }

}