package lua.lib.lrexlib;
@:luaRequire("rex_pcre")
extern class Rex {

	@:overload(          function       (expr : String, flag : Int) : Rex{})
	inline public static function create(expr : String, flag : String) : Rex{
		return untyped Rex['new'](expr, flag);
	}

	/**
	  The function searches for the first match of the regexp `patt` in the
	  string `subj`, starting from offset `init`, subject to flags `cf` and `ef`.
    
	  @return matched string, or array of strings.
	 **/
	public static function match(patt : String, ?init : Int, ?ef : Int) : Dynamic;

	/**
	 The function searches for the first match of the regexp patt in the string
	 `subj`, starting from offset `init`, subject to flags `cf` and `ef`. 
	 **/
	public static function find(subj : String, ?init : Int, ?ef : Int) : Dynamic;


	/**
	 The function is intended for use in the generic for Lua construct. It is
	 used for splitting a subject string `subj` into parts (sections). The `sep`
	 parameter is a regular expression pattern representing separators between
	 the sections. 
	 **/
	@:overload(   function      (subj : String, sep : Rex,    ?cf : Int, ?ef : Int) : Void->String{})
	public static function split(subj : String, sep : String, ?cf : Int, ?ef : Int) : Void->String;


	/**
	  This function counts matches of the pattern `patt` in the string `subj`.
	**/	
	public static function count(subj : String, patt : String, cf : Int, ef : Int) : Dynamic;
	public static function flags(?tb:Dynamic) : Dynamic;

  /**
    The function searches for the first match of the regexp in the string 
    `subj`, starting from offset `init`, subject to execution flags `ef`.
  **/
	public function tfind(subj : String, ?init : Int, ?ef : Int) : Dynamic;
	
	/**
	  This function searches for the first match of the regexp in the string 
	  `subj`, starting from offset `init`, subject to execution flags `ef`.
	**/
	public function exec(subj : String, ?init : Int, ?ef : Int) : Dynamic;

	/**
	 The function is intended for use in the generic for Lua construct. It
	 returns an iterator for repeated matching of the pattern patt in the
	 string `subj`, subject to flags `cf` and `ef`.
	 **/
	public static function gmatch(subj : String, ?cf : Int, ?ef : Int) : Dynamic; // TODO: Extern a lua iterator

	/**
	  This function searches for all matches of the pattern `patt` in the string 
    `subj` and replaces them according to the parameters `repl` and `n`.
	 **/
	@:overload(	  function     (subj : String, patt : Rex,    repl: Dynamic, ?n: Int, ?cf : Int, ?ef : Int) : String {})
	public static function gsub(subj : String, patt : String, repl: Dynamic, ?n: Int, ?cf : Int, ?ef : Int) : String;
}

