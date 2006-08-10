package flash.events;

extern interface IEventDispatcher {
	function new() : Void;
	function addEventListener(type : String, listener : Dynamic -> Void, ?useCapture : Bool, ?priority : Int, ?useWeakReference : Bool) : Void;
	function dispatchEvent(event : flash.events.Event) : Bool;
	function hasEventListener(type : String) : Bool;
	function removeEventListener(type : String, listener : Dynamic -> Void, ?useCapture : Bool) : Void;
	function willTrigger(type : String) : Bool;
}
