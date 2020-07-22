package lua.lib.lpeg;

@:luaRequire("lpeg")
extern class Lpeg {
    /**
      The matching function. It attempts to match the given pattern against the subject string. If the match succeeds, returns the index in the subject of the first character after the match, or the captured values (if the pattern captured any value).

      An optional numeric argument init makes the match start at that position in the subject string. As usual in Lua libraries, a negative value counts from the end.

      Unlike typical pattern-matching functions, match works only in anchored mode; that is, it tries to match the pattern with a prefix of the given subject string (at position init), not with an arbitrary substring of the subject. So, if we want to find a pattern anywhere in a string, we must either write a loop in Lua or write a pattern that matches anywhere. This second approach is easy and quite efficient;
     **/

    @:overload(function(subject:CapturePattern, ?init:Int) : Table<String,String> {})
    @:overload(function(subject:Pattern, ?init:Int) : Int {})
    @:overload(function(subject:Int, ?init:Int) : Int {})
    @:overload(function(subject:Bool, ?init:Int) : Bool {})
    @:overload(function(subject:Grammar, ?init:Int) : CapturePattern {})
    public function match(pattern: Pattern, subject:String, ?init:Int) : Int;


    /**
      If the given value is a pattern, returns the string "pattern". Otherwise returns nil.
     **/
    public static function type(value : Dynamic) : String;
    /**
      Returns a string with the running version of LPeg.
     **/
    public static function version() : String;

    /**
      Sets a limit for the size of the backtrack stack used by LPeg to track calls and choices. (The default limit is 400.) Most well-written patterns need little backtrack levels and therefore you seldom need to change this limit; before changing it you should try to rewrite your pattern to avoid the need for extra space. Nevertheless, a few useful patterns may overflow. Also, with recursive grammars, subjects with deep recursion may also need larger limits.
     **/
    public static function setmaxstack(max:Int) : Void;

    public static function locale(arg:Table<String,Pattern>) : Table<String,Pattern>;

}
