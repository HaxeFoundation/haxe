package flash.media;

extern class StageVideoAvailabilityReason {
	function new() : Void;
	static var DRIVER_TOO_OLD(default,never) : String;
	static var NO_ERROR(default,never) : String;
	static var UNAVAILABLE(default,never) : String;
	static var USER_DISABLED(default,never) : String;
	static var WMODE_INCOMPATIBLE(default,never) : String;
}
