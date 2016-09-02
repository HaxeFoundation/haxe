package flash.events;

@:require(flash10_1) extern class GeolocationEvent extends Event {
	var altitude : Float;
	var heading : Float;
	var horizontalAccuracy : Float;
	var latitude : Float;
	var longitude : Float;
	var speed : Float;
	var timestamp : Float;
	var verticalAccuracy : Float;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, latitude : Float = 0, longitude : Float = 0, altitude : Float = 0, hAccuracy : Float = 0, vAccuracy : Float = 0, speed : Float = 0, heading : Float = 0, timestamp : Float = 0) : Void;
	static var UPDATE(default,never) : String;
}
