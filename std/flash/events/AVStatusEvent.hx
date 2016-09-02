package flash.events;

extern class AVStatusEvent extends Event {
	var description(default,never) : String;
	var notificationType(default,never) : String;
	var result(default,never) : flash.media.AVResult;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, ?inNotificationType : String, inResult : Int = 0, ?inDescription : String) : Void;
	static var AV_STATUS(default,never) : String;
	static var BACKGROUND_MANIFEST_ERROR(default,never) : String;
	static var BACKGROUND_MANIFEST_WARNING(default,never) : String;
	static var BUFFER_STATE(default,never) : String;
	static var DECODER_TYPE(default,never) : String;
	static var DIMENSION_CHANGE(default,never) : String;
	static var ERROR(default,never) : String;
	static var INSERTION_COMPLETE(default,never) : String;
	static var LOAD_COMPLETE(default,never) : String;
	static var MANIFEST_UPDATE(default,never) : String;
	static var PLAY_STATE(default,never) : String;
	static var RENDER_TYPE(default,never) : String;
	static var SEEK_COMPLETE(default,never) : String;
	static var STEP_COMPLETE(default,never) : String;
	static var STREAM_SWITCH(default,never) : String;
	static var TRICKPLAY_ENDED(default,never) : String;
	static var WARNING(default,never) : String;
}
