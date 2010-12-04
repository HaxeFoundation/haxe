package flash.utils;

@:final extern class SetIntervalTimer extends Timer {
	var id : UInt;
	function new(closure : Dynamic, delay : Float, repeats : Bool, rest : Array<Dynamic>) : Void;
	@:require(flash10) function clearArrayEntry() : Void;
	static function clearInterval(id : UInt) : Void;
}
