package flash.events;

extern class FullScreenEvent extends flash.events.ActivityEvent {
		var fullScreen(default,null);
		function new(type:String, ?bubbles:Bool, ?cancelable:Bool, ?fullScreen:Bool );
		static var FULL_SCREEN : String;
}
