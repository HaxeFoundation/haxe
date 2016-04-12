package unit.issues;

private class StringOutput extends haxe.io.Output {
     var buf:StringBuf;

    public function new() {
        buf = new StringBuf();
    }

    override public function writeByte( c : Int ) : Void {
        buf.addChar(c);
    }

    public function toString() : String {
        return buf.toString();
    }
}

class Issue5086 extends Test {
    function test() {
        var so = new StringOutput();
        var input = "Hello.  I think Haxe is great!";
        so.writeString(input);
        eq(so.toString(), input);
    }
}
