package flash.display;

extern class Shape extends DisplayObject {
	@:flash.property var graphics(get,never) : Graphics;
	function new() : Void;
	private function get_graphics() : Graphics;
}
