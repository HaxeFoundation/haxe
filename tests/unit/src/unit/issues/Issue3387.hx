package unit.issues;

class Issue3387 extends unit.Test {
    function test() {
        eq("#pos(some:1: lines 1-4)", getPos("{\n1;\n2;\n3;}", "some"));
    }

    static macro function getPos(source:String, file:String) {
        var e = haxe.macro.Context.parseInlineString(source, haxe.macro.Context.makePosition({min: 0, max: 0, file: file}));
        return macro $v{Std.string(e.pos)};
    }
}