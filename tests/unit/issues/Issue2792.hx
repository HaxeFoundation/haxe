package unit.issues;
import unit.Test;

private class Obj {
    public var w:Float;
    public var h:Float;
    public function new(w:Float = 0, h:Float = 0) {
        this.w = w;
        this.h = h;
    }
}

private class Node {
    public var obj:Obj;

    public function new() { }

    public inline function canPlace(w:Float, h:Float):Bool {
        return (obj.w == w && obj.h == h);
    }
}


private class Root {
    public var left:Node;
    public var right:Node;

    public function new() {}
}

class Issue2792 extends Test {
	function test() {
        var root:Root = new Root();
        root.left = new Node();
        root.left.obj = new Obj();
        var check1:Bool = (root.right != null && root.right.canPlace(0, 0)) && (root.left != null && root.left.canPlace(0, 0));
        eq(false, check1);
        var check2:Bool = (root.left != null && root.left.canPlace(0, 0)) && (root.right != null && root.right.canPlace(0, 0));
        eq(false, check2);
        var right:Bool = (root.right != null && root.right.canPlace(0, 0));
        var left:Bool = (root.left != null && root.left.canPlace(0, 0));
        var both:Bool = right && left;
        eq(false, both);
	}
}