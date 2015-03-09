package lua;
@:luaRequire("rex_pcre")
extern class Rex {
	public function new(expr : String, flag : String);

	/**
	  The function searches for the first match of the regexp patt in the
	  string subj, starting from offset init, subject to flags cf and ef.
	  Returns matched string, or array of strings.
	 **/
	public function match(patt : String, ?init : Int, ?ef : Int) : Dynamic;

	/**
	 The function searches for the first match of the regexp patt in the string
	 subj, starting from offset init, subject to flags cf and ef. 
	 Returns 
	 **/
	public function find(subj : String, ?init : Int, ?ef : Int) : Dynamic;


	/**
	 The function is intended for use in the generic for Lua construct. It is
	 used for splitting a subject string subj into parts (sections). The sep
	 parameter is a regular expression pattern representing separators between
	 the sections. 
	 **/
	public function split(subj : String, sep : String, cf : Int, ef : Int) : lua.Table<Int,String>;


	/**
	  This function counts matches of the pattern patt in the string subj.
	**/	
	public function count(subj : String, patt : String, cf : Int, ef : Int) : Dynamic;

	public function flags(tb:Dynamic) : Dynamic;

	public function tfind(subj : String, ?init : Int, ?ef : Int) : Dynamic;

	public function exec(subj : String, ?init : Int, ?ef : Int) : Dynamic;

	/**
	 The function is intended for use in the generic for Lua construct. It
	 returns an iterator for repeated matching of the pattern patt in the
	 string subj, subject to flags cf and ef.
	 **/
	public static function gmatch(subj : String, ?cf : Int, ?ef : Int) : lua.Iterator;

	/**
	  This function searches for all matches of the pattern patt in the string subj
	  and replaces them according to the parameters repl and n.
	 **/
	public static function gsub(subj : String, patt : String, repl: Dynamic, ?n: Int, ?cf : Int, ?ef : Int) : String;
}

