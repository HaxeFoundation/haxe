package lua.lib.lpeg;
import haxe.extern.EitherType;
import haxe.extern.Rest;
import lua.Table;
import haxe.Constraints.Function;
import haxe.ds.StringMap;



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
    public inline function repeat(times:Int) : Pattern {
        return untyped __lua__("{}^{}", this, time);
    }

}

extern class BeforePattern extends Pattern {
    @:native("B")
    public function new(value : PatternArgument);
}

extern class RangePattern extends Pattern {
    @:native("R")
    public function new(args:Rest<String>);
}

extern class StringPattern extends Pattern {
    @:native("S")
    public function new(arg:String);
}

extern class VariablePattern extends Pattern {
    @:native("V")
    public function new(arg:String);
}

extern class CapturePattern extends Pattern {
    @:native("C")
    public function new(pattern:CapturePattern);
}

extern class CaptureArgPattern extends Pattern {
    @:native("Carg")
    public function new(n:Int);
}

extern class CaptureBeforePattern extends Pattern {
    @:native("Cb")
    public function new(name:String);
}

extern class CaptureValuesPattern extends Pattern {
    @:native("Cc")
    public function new(values : String);
}

extern class CaptureFoldingPattern extends Pattern {
    @:native("Cf")
    public function new(pattern:Pattern, func: String->Dynamic);
}

extern class CaptureTaggedPattern extends Pattern {
    @:native("Cg")
    public function new(pattern:Pattern, ?name:String);
}

extern class CapturePosition extends Pattern {
    @:native("Cp")
    public function new();
}



typedef PatternArgument = EitherType<Pattern, EitherType<String, EitherType<Int, EitherType<Bool, EitherType<AnyTable, Function>>>>>;

abstract Grammar(Table<String,String>) to Table<String,String>{
    public function new(name:String, rules : StringMap<String>) {
       this = Table.fromMap(rules);
       this[0] = name;
    }
}
