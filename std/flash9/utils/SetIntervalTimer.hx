package flash.utils;

@:final extern class SetIntervalTimer extends Timer {
	var id : UInt;
	function new(closure : Dynamic, delay : Float, repeats : Bool, rest : Array<Dynamic>) : Void;
	static function clearInterval(id_to_clear : UInt) : Void;
}
