package flash.media;

extern class AVTrackInfo {
	var activity(default,never) : Bool;
	var autoSelect(default,never) : Bool;
	var dataTrackInfoServiceType(default,never) : String;
	var defaultTrack(default,never) : Bool;
	var description(default,never) : String;
	var forced(default,never) : Bool;
	var language(default,never) : String;
	var pid(default,never) : Int;
	function new(init_description : String, init_language : String, init_defaultTrack : Bool, init_autoSelect : Bool, init_forced : Bool, init_activity : Bool, init_dataTrackInfoServiceType : String, init_pid : Int) : Void;
	static final DTI_608_CAPTIONS : String;
	static final DTI_708_CAPTIONS : String;
	static final DTI_WEBVTT_CAPTIONS : String;
}
