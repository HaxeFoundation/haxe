package flash.utils;

extern class SetIntervalTimer extends flash.utils.Timer {
	function new(closure : Dynamic, delay : Float, repeats : Bool, rest : Array<Dynamic>) : Void;
	private var closure : Dynamic;
	private var id : UInt;
	private function onTimer(event : flash.events.Event) : Void;
	private var rest : Array<Dynamic>;
	private static function clearInterval(id : UInt) : Void;
	private static var intervals : Array<Dynamic>;
}
