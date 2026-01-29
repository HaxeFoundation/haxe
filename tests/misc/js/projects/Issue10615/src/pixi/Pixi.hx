package pixi;

@:js.import("../pixi.js")
extern class Application {
	function new();
	function test():String;
	static function name():String;
}

@:js.import(pixijs_path, "Application")
extern class Application2 {
	function new();
	function test():String;
	static function name():String;
}

@:js.import("../pixi.js", "Application")
extern class Application3 {
	function new();
	function test():String;
	static function name():String;
}

@:js.import(@default "../pixi.js")
extern class PixiDef {
	static function name():String;
}

@:js.import("../pixi.js", "Assets")
extern class Assets {
	static function load<T>(url:String):String;
}
