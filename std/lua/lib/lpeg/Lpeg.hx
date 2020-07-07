package lua.lib.lpeg;

@:luaRequire("lpeg")
extern class Lpeg {
    public static function match(pattern : Pattern, subject:String, ?init:Int) : Matched;

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
