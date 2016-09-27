package js.jquery;

class JqIterator {
	var j:JQuery;
	var i:Int;
	inline public function new(j:JQuery):Void {
		this.i = 0;
		this.j = j;
	}
	inline public function hasNext():Bool {
		return i < j.length;
	}
	inline public function next():js.html.Element {
		return this.j[i++];
	}

	static function __init__() {
		if (untyped __typeof__(JQuery) != "undefined" && JQuery.fn != null)
			JQuery.fn.iterator = function() return new JqIterator(js.Lib.nativeThis);
	}
}