package unit.issues;

private class TestClass {
	public var width (get,set) : Metric;
	var _width          : Metric;

	public function new() {
		_width = new MetricCore();
	}

	function get_width() return _width;
	function set_width(w:Metric) return _width = w;
}


class Issue3717 extends unit.Test {
	function test() {
		var t = new TestClass();
		t.width += 10;
		feq(10, t.width.dip);
		t.width += 5;
		feq(15, t.width.dip);
	}
}

@:forward
private abstract Metric (MetricCore) from MetricCore to MetricCore {
	@:from static function fromFloat(v:Float) {
		var m = new MetricCore();
		m.dip = v;
		return m;
	}
	@:to function toFloat() return this.dip;


	@:op(A += B) static function AincBf (a:Metric, b:Float) return a.dip += b;
}


private class MetricCore {
	public var dip : Float = 0;

	public function new () : Void {}
}