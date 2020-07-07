package lpeg;
import haxe.extern.EitherType;
import lua.Table;
import haxe.Constraints.Function;

@:luaRequire("lpeg.P")
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
    public function new(value : PatternArgument);
    /**
        The matching function. It attempts to match the given pattern against the subject string. If the match succeeds, returns the index in the subject of the first character after the match, or the captured values (if the pattern captured any value).

        An optional numeric argument init makes the match start at that position in the subject string. As usual in Lua libraries, a negative value counts from the end.

        Unlike typical pattern-matching functions, match works only in anchored mode; that is, it tries to match the pattern with a prefix of the given subject string (at position init), not with an arbitrary substring of the subject. So, if we want to find a pattern anywhere in a string, we must either write a loop in Lua or write a pattern that matches anywhere. This second approach is easy and quite efficient;
     **/
    public function match(subject:String, ?init:Int) : Matched;

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

}

typedef PatternArgument = EitherType<Pattern, EitherType<String, EitherType<Int, EitherType<Bool, EitherType<AnyTable, Function>>>>>;

abstract Matched(Int) {
    inline public function new(d:Dynamic) {
        this = d;
    }
    inline public function matched() : Bool {
        return Std.isOfType(this, Int) || Lua.next(cast this) != null;
    }
    inline public function captures() : Table<String,String> {
        if (Std.isOfType(this,Table)) {
            return cast this;
        } else {
            return Table.create();
        }
    }
    inline public function position() : Int {
        if (Std.isOfType(this, Int)) {
            return this;
        } else {
            return null;
        }
    }
}

