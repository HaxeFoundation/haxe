package lua.lib.lpeg;
import lua.Table;
import haxe.ds.StringMap;
import haxe.extern.Rest;
import haxe.Constraints.Function;
import lua.lib.lpeg.Pattern.PatternMetatable as M;

@:luaRequire("lpeg")
extern class PatternMetatable {
    static public function __add(p1:Pattern, p2:AbstractPattern) : AbstractPattern;
    static public function __mul(p1:Pattern, n:Int) : AbstractPattern;
    static public function __pow(p1:Pattern, n:Int) : AbstractPattern;
    static public function __div(p1:Pattern, n:Dynamic) : AbstractPattern;
    static inline function __init__() : Void {
        untyped PatternMetatable = __lua__("getmetatable(require('lpeg').P(1))");
    }
}


@:forward(match)
abstract AbstractPattern(Pattern){
    @:op(A + B)
    public inline function add(p:AbstractPattern) : AbstractPattern  return M.__add(this, p);

    @:op(A * B)
    public inline function mul(n:Int) : AbstractPattern  return M.__pow(this, n);

    @:op(A / B)
    public inline function divf(f:Function) : AbstractPattern  return M.__div(this, f);

    public inline function len() : AbstractPattern { return untyped __lua_length__(this);}
}


@:luaRequire("lpeg")
extern class Pattern {
    /**
      Converts the given value into a proper pattern, according to the following rules:
        * If the argument is a pattern, it is returned unmodified.
        * If the argument is a string, it is translated to a pattern that matches the string literally.
        * If the argument is a non-negative number n, the result is a pattern that matches exactly n characters.
        * If the argument is a negative number -n, the result is a pattern that succeeds only if the input string has less than n characters left: lpeg.P(-n) is equivalent to -lpeg.P(n) (see the unary minus operation).
        * If the argument is a boolean, the result is a pattern that always succeeds or always fails (according to the boolean value), without consuming any input.
        * If the argument is a table, it is interpreted as a grammar (see Grammars).
        * If the argument is a function, returns a pattern equivalent to a match-time capture over the empty string.
     **/

    /**
      The matching function. It attempts to match the given pattern against the subject string. If the match succeeds, returns the index in the subject of the first character after the match, or the captured values (if the pattern captured any value).

      An optional numeric argument init makes the match start at that position in the subject string. As usual in Lua libraries, a negative value counts from the end.

      Unlike typical pattern-matching functions, match works only in anchored mode; that is, it tries to match the pattern with a prefix of the given subject string (at position init), not with an arbitrary substring of the subject. So, if we want to find a pattern anywhere in a string, we must either write a loop in Lua or write a pattern that matches anywhere. This second approach is easy and quite efficient;
     **/
    public static function R(args:Rest<String>) : AbstractPattern;
    public function match(subject:String, ?init:Int) : Int;
    public function __add(p2:AbstractPattern) : AbstractPattern;
    public function __mul(n:Int) : AbstractPattern;
    public function __div(n:Dynamic) : AbstractPattern;
}

