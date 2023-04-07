package lua.lib.luautf8;

/**
	These are all externs for the lua-utf8 library, which functions
	as an additional set of string tools.

	Note that all relevant indexes are "1" based.
**/
@:luaRequire('lua-utf8')
extern class Utf8 {
	/**
		Receives a string and returns its length. The empty string `""` has
		length `0`. Embedded zeros are counted, so `"a\000bc\000"` has length `5`.
	**/
	static function len(str:String):Int;

	/**
		Receives zero or more integers. Returns a string with length equal to the
		number of arguments, in which each character has the internal numerical
		code equal to its corresponding argument.
		Note that numerical codes are not necessarily portable across platforms.
	**/
	static function char(codes:haxe.extern.Rest<Int>):String;

	/**
		Returns the substring of `str` that starts at `start` and continues until `end`;
		`start` and `end` can be negative. If `end` is absent, then it is assumed to be
		equal to `-1` (which is the same as the string length).
		In particular, the call `sub(str,1,end)` returns a prefix of `str`
		with length `end`, and `sub(str, -end)` returns a suffix of `str` with
		length `start`.
	**/
	static function sub(str:String, start:Int, ?end:Int):StringSub;

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
	static function find(str:String, target:String, ?start:Int, ?plain:Bool):StringFind;

	/**
		Returns the internal numerical codes of the characters `str[index]`.
		Note that numerical codes are not necessarily portable across platforms.
	**/
	static function byte(str:String, ?index:Int):Int;

	/**

	**/
	@:overload(function(str:String, pattern:String, replace:String->Void, ?n:Int):String {})
	@:overload(function(str:String, pattern:String, replace:String->String, ?n:Int):String {})
	static function gsub(str:String, pattern:String, replace:String, ?n:Int):String;

	/**
		Returns an iterator function that, each time it is called, returns the next
		captures from pattern over string `str`. If `pattern` specifies no captures,
		then the whole match is produced in each call.
	**/
	@:overload(function(str:String, pattern:String, match:Void->String, ?n:Int):String->Void {})
	static function gmatch(str:String, pattern:String):Void->String;

	/**
		Looks for the first match of pattern in the string s. If it finds one,
		then match returns the captures from the pattern; otherwise it returns `null`.
		If pattern specifies no captures, then the whole match is returned.
		The optional argument `n` specifies where to start the search;
		its default value is `1` and can be negative.
	**/
	static function match(str:String, pattern:String, ?n:Int):String;

	/**
		Receives a string and returns a copy of this string with all lowercase
		letters changed to uppercase. All other characters are left unchanged.
		The definition of what a lowercase letter is depends on the current locale.
	**/
	static function upper(str:String):String;

	/**
		Receives a string and returns a copy of this string with all uppercase
		letters changed to lowercase. All other characters are left unchanged.
		The definition of what an uppercase letter is depends on the current locale.
	**/
	static function lower(str:String):String;

	static function codes(str:String):String->Int->StringCodePoint;
}

@:multiReturn extern class StringFind {
	var begin:Int;
	var end:Int;
}

@:multiReturn extern class StringSub {
	var match:String;
	var count:Int;
}

@:multiReturn extern class StringCodePoint {
	var position:Int;
	var codepoint:Int;
}
