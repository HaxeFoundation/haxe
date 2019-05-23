package flash.events;

extern class AVStatusEvent extends Event {
	@:flash.property var description(get,never) : String;
	@:flash.property var notificationType(get,never) : String;
	@:flash.property var result(get,never) : flash.media.AVResult;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, ?inNotificationType : String, inResult : Int = 0, ?inDescription : String) : Void;
	private function get_description() : String;
	private function get_notificationType() : String;
	private function get_result() : flash.media.AVResult;
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
