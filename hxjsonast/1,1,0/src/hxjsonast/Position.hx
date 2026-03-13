package hxjsonast;

/**
    Position of a JSON value.

    This is compatible with `haxe.macro.Expr.Position`
    and can be used for `haxe.macro.Context.makePosition`.
**/
@:structInit
class Position {
    /**
        The name of a JSON file.
    **/
    public var file:String;

    /**
        Starting positon of the value, zero-based.
    **/
    public var min:Int;

    /**
        Ending position of the value, zero-based.
    **/
    public var max:Int;

    public inline function new(file:String, min:Int, max:Int) {
        this.file = file;
        this.min = min;
        this.max = max;
    }
}
