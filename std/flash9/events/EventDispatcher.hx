package flash.events;

extern class EventDispatcher implements IEventDispatcher {
	function new(?target : flash.events.IEventDispatcher) : Void;
	function addEventListener(type : String, listener : Dynamic -> Void, ?useCapture : Bool, ?priority : Int, ?useWeakReference : Bool) : Void;
	function dispatchEvent(event : Event) : Bool;
	function hasEventListener(type : String) : Bool;
	function removeEventListener(type : String, listener : Dynamic -> Void, ?useCapture : Bool) : Void;
	function toString() : String;
	function willTrigger(type : String) : Bool;
}
