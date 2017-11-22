package js.jquery;

class JqEltsIterator {
	var j:JQuery;
	var i:Int;
	inline public function new(j:JQuery):Void {
		this.i = 0;
		this.j = j;
	}
	inline public function hasNext():Bool {
		return i < j.length;
	}
	inline public function next():JQuery {
		return new JQuery(this.j[i++]);
	}

	static function __init__() {
		if (js.Syntax.typeof(JQuery) != "undefined" && JQuery.fn != null)
			JQuery.fn.elements = function() return new JqEltsIterator(js.Lib.nativeThis);
	}
}