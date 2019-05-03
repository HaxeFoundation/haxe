package flash.media;

extern class AVTrackInfo {
	var activity(get,never) : Bool;
	var autoSelect(get,never) : Bool;
	var dataTrackInfoServiceType(get,never) : String;
	var defaultTrack(get,never) : Bool;
	var description(get,never) : String;
	var forced(get,never) : Bool;
	var language(get,never) : String;
	var pid(get,never) : Int;
	function new(init_description : String, init_language : String, init_defaultTrack : Bool, init_autoSelect : Bool, init_forced : Bool, init_activity : Bool, init_dataTrackInfoServiceType : String, init_pid : Int) : Void;
	private function get_activity() : Bool;
	private function get_autoSelect() : Bool;
	private function get_dataTrackInfoServiceType() : String;
	private function get_defaultTrack() : Bool;
	private function get_description() : String;
	private function get_forced() : Bool;
	private function get_language() : String;
	private function get_pid() : Int;
	static final DTI_608_CAPTIONS : String;
	static final DTI_708_CAPTIONS : String;
	static final DTI_WEBVTT_CAPTIONS : String;
}
