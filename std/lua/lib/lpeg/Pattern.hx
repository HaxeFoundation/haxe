package lua.lib.lpeg;
import lua.lib.lpeg.Pattern.PatternMetatable as M;

import haxe.extern.Rest;
import haxe.extern.EitherType as E;
import lua.Table;
import haxe.Constraints.Function;

typedef PatternArgument = E<Pattern, E<String, E<Int, E<Bool, E<Table<String,String>, Function>>>>>;

@:luaRequire("lpeg")
extern class PatternMetatable {
    static public function __add(v1:Dynamic, v2:Dynamic) : Pattern;
    static public function __mul(v1:Dynamic, v2:Dynamic) : Pattern;
    static public function __pow(v1:Dynamic, v2:Dynamic) : Pattern;
    static public function __div(v1:Dynamic, v2:Dynamic) : Pattern;
    static inline function __init__() : Void {
        untyped PatternMetatable = __lua__("getmetatable(require('lpeg').P(1))");
    }
}

@:forward(match)
abstract Pattern(PatternImpl){
    @:op(A + B)
    public static inline function add(p1:Pattern, p2 : Pattern) : Pattern  return M.__mul(p1, p2);

    @:op(A + B)
    public static inline function addString(p : Pattern, str:String) : Pattern  return M.__mul(p, str);

    @:op(A + B)
    public static inline function addStringPre(str : String, p : Pattern) : Pattern  return M.__mul(str, p);

    @:op(A | B)
    public static inline function or(p1:Pattern, p2:Pattern) : Pattern  return M.__add(p1, p2);

    @:op(A | B)
    public static inline function orString(p:Pattern, s:String) : Pattern  return M.__add(p, s);

    @:op(A | B)
    public static inline function orStringPre(s:String, p:Pattern) : Pattern  return M.__add(s, p);

    @:op(A * B)
    public static inline function mul(p: Pattern, n:Int) : Pattern  return M.__pow(p, n);

    @:op(A >> B)
    public static inline function divf(p : Pattern, f:Function) : Pattern  return M.__div(p, f);

    @:op(A >> B)
    public static inline function divi(p : Pattern, i:Int) : Pattern  return M.__div(p, i);

    public inline function len() : Pattern { return untyped __lua_length__(this);}

    inline public static function P(p:PatternArgument) : Pattern  return PatternImpl.P(p);
    inline public static function R(arg:String) : Pattern return PatternImpl.R(arg);
    inline public static function Cf(p:Pattern, f : Function) : Pattern return PatternImpl.Cf(p, f);
    inline public static function Ct(p:Pattern) : Table<Dynamic, String> return PatternImpl.Ct(p);
    inline public static function Cc(val:Dynamic) : Pattern return PatternImpl.Cc(val);
    inline public static function Ca(p:Pattern) : Pattern {
        return Cf(Cc([]) + p, (acc:Array<String>, v : Dynamic)-> {
            acc.push(v);
            return acc;
        });
    }
}




@:luaRequire("lpeg")
extern class PatternImpl {
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
    public static function P(p:PatternArgument) : Pattern;
    public static function R(args:Rest<String>) : Pattern;
    public static function Cf(p:Pattern, f : Function) : Pattern;
    public static function Cc(val:Rest<Dynamic>) : Pattern;

    public static function Ct(p:Pattern) : Table<String,String>;
    public function match(subject:String, ?init:Int) : Dynamic;
}

