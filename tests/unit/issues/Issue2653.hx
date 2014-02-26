package unit.issues;
import unit.Test;

private class BasicAsset {
	public var s:String;
	function new (s:String) {
		this.s = s;
	}
}

@:generic
private class BasicTexture<R> extends BasicAsset {
	public var i:Int;
	public function new(i:Int, s:String) {
		super(s);
		this.i = i;
    }
}

private class CanvasTexture extends BasicTexture<Int> {
    public function new(i:Int, s:String) {
		super(i, s);
    }
}

class Issue2653 extends Test {
	function test() {
		var ct = new CanvasTexture(12, "foo");
		eq(ct.i, 12);
		eq(ct.s, "foo");
	}
}