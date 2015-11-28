package flash.media;

extern class AVTrackInfo {
	var activity(default,null) : Bool;
	var autoSelect(default,null) : Bool;
	var dataTrackInfoServiceType(default,null) : String;
	var defaultTrack(default,null) : Bool;
	var description(default,null) : String;
	var forced(default,null) : Bool;
	var language(default,null) : String;
	var pid(default,null) : Int;
	function new(init_description : String, init_language : String, init_defaultTrack : Bool, init_autoSelect : Bool, init_forced : Bool, init_activity : Bool, init_dataTrackInfoServiceType : String, init_pid : Int) : Void;
	static var DTI_608_CAPTIONS : String;
	static var DTI_708_CAPTIONS : String;
	static var DTI_WEBVTT_CAPTIONS(default,never) : String;
}
