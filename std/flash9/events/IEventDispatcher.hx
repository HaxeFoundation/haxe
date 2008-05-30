package flash.events;

extern interface IEventDispatcher {
	function addEventListener(type : String, listener : Dynamic -> Void, ?useCapture : Bool, ?priority : Int, ?useWeakReference : Bool) : Void;
	function dispatchEvent(event : Event) : Bool;
	function hasEventListener(type : String) : Bool;
	function removeEventListener(type : String, listener : Dynamic -> Void, ?useCapture : Bool) : Void;
	function willTrigger(type : String) : Bool;
}
