package lua.lib.lpeg;
import haxe.extern.EitherType;
import haxe.extern.Rest;
import lua.Table;
import haxe.Constraints.Function;



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
    @:native("P")
    public function new(value : PatternArgument) : Void;
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
    public function match(subject:String, ?init:Int) : Int;

    public inline function and() : Pattern {
        return untyped __lua__("#{}", this);
    }

    public inline function invert() : Pattern {
        return untyped __lua__("-{}", this);
    }

    public inline function either(p:Pattern) : Pattern {
        return untyped __lua__("{} + {}", this, p);
    }

    public inline function not(p:Pattern) : Pattern {
        return untyped __lua__("{} - {}", this, p);
    }

    public inline function next(p:Pattern) : Pattern {
        return untyped __lua__("{} - {}", this, p);
    }
    public inline function times(times:Int) : Pattern {
        return untyped __lua__("{}^{}", this, time);
    }

}

extern class BeforePattern extends Pattern {
    @:native("B")
    public function new(value : PatternArgument) : Void;
}

extern class RangePattern extends Pattern {
    @:native("R")
    public function new(args:Rest<String>) : Void;
}

extern class StringPattern extends Pattern {
    @:native("S")
    public function new(arg:String) : Void;
}

extern class VariablePattern extends Pattern {
    @:native("V")
    public function new(arg:String) : Void;
}

extern class CapturePattern extends Pattern {
}

typedef PatternArgument = EitherType<Pattern, EitherType<String, EitherType<Int, EitherType<Bool, EitherType<AnyTable, Function>>>>>;
