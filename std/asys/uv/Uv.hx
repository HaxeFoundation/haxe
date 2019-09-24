package asys.uv;

extern class Uv {
	static function init():Void;
	static function run(mode:asys.uv.UVRunMode):Bool;
	static function stop():Void;
	static function close():Void;

	static function __init__():Void {
		Uv.init();
	}
}
