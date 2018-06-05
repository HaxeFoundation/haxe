package unit.issues.misc;

@:callable
abstract Issue5009Assert(Bool->Void) {
    public static var ASSert = new Issue5009Assert();
    function new() this = function(b) if (!b) throw "assert";
}