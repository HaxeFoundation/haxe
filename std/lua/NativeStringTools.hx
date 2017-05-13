package lua;
/**
	These are all externs for the base Lua "string" class, which functions 
	as an additional set of string tools.

	Note that all relevant indexes are "1" based.
**/
@:native("_G.string")
extern class NativeStringTools {
	/**
		Receives a string and returns its length. The empty string `""` has 
		length `0`. Embedded zeros are counted, so `"a\000bc\000"` has length `5`.
	**/
	public static function len(str : String): Int;

	/**
		Receives zero or more integers. Returns a string with length equal to the 
		number of arguments, in which each character has the internal numerical 
		code equal to its corresponding argument.
		Note that numerical codes are not necessarily portable across platforms.
	**/
	public static function char(codes: haxe.extern.Rest<Int>): String;


	// TODO: make a note about handling matched groups with multireturn
	/**
		Returns the substring of `str` that starts at `start` and continues until `end`; 
		`start` and `end` can be negative. If `end` is absent, then it is assumed to be 
		equal to `-1` (which is the same as the string length). 
		In particular, the call `sub(str,1,end)` returns a prefix of `str` 
		with length `end`, and `sub(str, -end)` returns a suffix of `str` with 
		length `start`.
	**/
	public static function sub(str : String, start : Int, ?end : Int): StringSub;

	/**
		Returns the character code at position `index` of `str`.
	**/
	public static function charCodeAt(str : String, index : Int): Int;

	/**
		Looks for the first match of pattern in the string `str`. 
		If it finds a match, then `find` returns the indices of `str` where this 
		occurrence starts and ends.
		
		@param target If the target has captures, then in a successful match the 
		       captured values are also returned, after the two indices.
		@param start specifies where to start the search; its default value is `1`
		       and can be negative. 
		@param plain turns off the pattern matching facilities, so the function does 
		       a plain "find substring" operation, with no characters in pattern 
		       being considered "magic". Note that if plain is given, then `start` must be given as well.
	**/
	public static function find(str : String, target : String, ?start : Int, ?plain : Bool): StringFind;

	/**
		Returns the internal numerical codes of the characters `str[index]`.
		Note that numerical codes are not necessarily portable across platforms.
	**/
	public static function byte(str : String, ?index : Int) : Int;

	/**
		Returns a formatted version of its variable number of arguments following 
		the description given in its first argument (which must be a string). 
		The format string follows the same rules as the printf family of standard C 
		functions. The only differences are that the options/modifiers 
		`*`, `l`, `L`, `n`, `p`, and `h` are not supported and that there is an 
		extra option, `q`. The `q` option formats a string in a form suitable to be
		safely read back by the Lua interpreter: the string is written between 
		double quotes, and all double quotes, newlines, embedded zeros, 
		and backslashes in the string are correctly escaped when written.
		For instance, the call
   `string.format('%q', 'a string with "quotes" and \n new line')`
		will produce the string:
		`"a string with \"quotes\" and \
      new line"`
		
		The options `c`, `d` `E`, `e`, `f`, `g`, `G`, `i`, `o`, `u, `X-, and `x` all 
		expect a number as argument, whereas `q` and `s` expect a string.
		
		This function does not accept string values containing embedded zeros, 
		except as arguments to the `q` option.
	**/
	public static function format(str : String, ?e1 : Dynamic, ?e2 : Dynamic, ?e3 : Dynamic, ?e4 : Dynamic): String;

	/**
		
	**/
	@:overload(   function     (str : String, pattern : String, replace : String->Void,   ?n : Int): String {})
	@:overload(   function     (str : String, pattern : String, replace : String->String, ?n : Int): String {})
	public static function gsub(str : String, pattern : String, replace : String,		  ?n : Int): String;

	/**
		Returns an iterator function that, each time it is called, returns the next 
		captures from pattern over string `str`. If `pattern` specifies no captures, 
		then the whole match is produced in each call.
	**/
	@:overload(   function     (str : String, pattern : String, match : Void->String,   ?n : Int): String->Void {})
	public static function gmatch(str : String, pattern : String): Void->String;

	/**
		Looks for the first match of pattern in the string s. If it finds one, 
		then match returns the captures from the pattern; otherwise it returns `null`.
		If pattern specifies no captures, then the whole match is returned.
		The optional argument `n` specifies where to start the search; 
		its default value is `1` and can be negative.
	**/
	public static function match(str : String, pattern : String, ?n : Int): String;

	/**
		Receives a string and returns a copy of this string with all lowercase 
		letters changed to uppercase. All other characters are left unchanged. 
		The definition of what a lowercase letter is depends on the current locale.
	**/
	public static function upper(str:String) : String;

	/**
		Receives a string and returns a copy of this string with all uppercase 
		letters changed to lowercase. All other characters are left unchanged. 
		The definition of what an uppercase letter is depends on the current locale.
	**/
	public static function lower(str:String) : String;

	/**
		Returns a string containing a binary representation of the given function,
		so that a later loadstring on this string returns a copy of the function.
		function must be a Lua function without upvalues.
	**/
	public static function dump(d:Dynamic) : Dynamic;
}

@:multiReturn extern class StringFind {
	var begin : Int;
	var end : Int;
}

@:multiReturn extern class StringSub {
	var match : String;
	var count : Int;
}
