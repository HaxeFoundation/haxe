package flash.events;

extern interface IEventDispatcher {
	function addEventListener(type : String, listener : Dynamic -> Void, useCapture : Bool = false, priority : Int = 0, useWeakReference : Bool = false) : Void;
	function dispatchEvent(event : Event) : Bool;
	function hasEventListener(type : String) : Bool;
	function removeEventListener(type : String, listener : Dynamic -> Void, useCapture : Bool = false) : Void;
	function willTrigger(type : String) : Bool;
}
