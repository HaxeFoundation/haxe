package flash.events;

extern class AVStatusEvent extends Event {
	var description(default,null) : String;
	var notificationType(default,null) : String;
	var result(default,null) : flash.media.AVResult;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, ?inNotificationType : String, inResult : Int = 0, ?inDescription : String) : Void;
	static var AV_STATUS : String;
	static var BUFFER_STATE : String;
	static var DECODER_TYPE : String;
	static var DIMENSION_CHANGE : String;
	static var ERROR : String;
	static var INSERTION_COMPLETE : String;
	static var LOAD_COMPLETE : String;
	static var MANIFEST_UPDATE : String;
	static var PLAY_STATE : String;
	static var RENDER_TYPE : String;
	static var SEEK_COMPLETE : String;
	static var STEP_COMPLETE : String;
	static var STREAM_SWITCH : String;
	static var WARNING : String;
}
