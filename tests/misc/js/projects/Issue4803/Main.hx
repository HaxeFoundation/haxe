@:native("$.Event") extern class Event extends js.html.Event {
	public var data : Dynamic;	
} 

@:native("$") extern class JQuery implements ArrayAccess<js.html.Element> {
	public function new():Void;
	
	@:overload(function(handler:Event -> Void):JQuery { })	
	@:overload(function(?eventData:Dynamic, handler:Event -> Void):JQuery { })	
	public function click():JQuery;
}

class Main  {
	static function main() {
		var j = new JQuery();
		j.click(function(e) {
			var p = { y : 3 };
			p = { x : 5 };
		});
	}
}