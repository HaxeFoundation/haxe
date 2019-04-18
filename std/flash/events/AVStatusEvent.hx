package flash.events;

extern class AVStatusEvent extends Event {
	var description(default,never) : String;
	var notificationType(default,never) : String;
	var result(default,never) : flash.media.AVResult;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, ?inNotificationType : String, inResult : Int = 0, ?inDescription : String) : Void;
	static final AV_STATUS : String;
	static final BACKGROUND_MANIFEST_ERROR : String;
	static final BACKGROUND_MANIFEST_WARNING : String;
	static final BUFFER_STATE : String;
	static final DECODER_TYPE : String;
	static final DIMENSION_CHANGE : String;
	static final ERROR : String;
	static final INSERTION_COMPLETE : String;
	static final LOAD_COMPLETE : String;
	static final MANIFEST_UPDATE : String;
	static final PLAY_STATE : String;
	static final RENDER_TYPE : String;
	static final SEEK_COMPLETE : String;
	static final STEP_COMPLETE : String;
	static final STREAM_SWITCH : String;
	static final TRICKPLAY_ENDED : String;
	static final WARNING : String;
}
