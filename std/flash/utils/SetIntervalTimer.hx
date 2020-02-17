package flash.utils;

extern final class SetIntervalTimer extends Timer {
	@:ns("flash.utils",internal) var id : UInt;
	function new(closure : Dynamic, delay : Float, repeats : Bool, rest : Array<Dynamic>) : Void;
	@:ns("flash.utils",internal) static function clearInterval(id_to_clear : UInt) : Void;
}
