package unit.issues;

private enum E {
	Tiles(x:Dynamic);
	TileInstances(x:Dynamic);
}

@:keep
private class Content {
	public var find:String -> Content;
	public function new() {
		find = function(s:String) return this;
	}

	public function spectrum(s:String, d:Dynamic) {
		return this;
	}

	public function closest(s:String) {
		return this;
	}

	public function css(s:Dynamic) {
		return "foo";
	}
}

class Issue5082 extends unit.Test {
	function test() {
		var l = {data: Tiles([]), props:{color:""}, idToIndex: null};
		var content = new Content();
		var x = (untyped content.find("[name=color]")).spectrum("set", toColor(l.props.color)).closest(".item").css( { display : l.idToIndex == null && !l.data.match(Tiles(_) | TileInstances(_)) ? "" : "none" } );
		eq("foo", x);
	}

	static function toColor(d:Dynamic) return d;
}