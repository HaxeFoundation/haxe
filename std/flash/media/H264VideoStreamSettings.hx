package flash.media;

extern class H264VideoStreamSettings extends VideoStreamSettings {
	var level(default,never) : String;
	var profile(default,never) : String;
	function new() : Void;
	function setProfileLevel(profile : String, level : String) : Void;
}
