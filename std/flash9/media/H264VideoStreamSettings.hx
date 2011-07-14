package flash.media;

extern class H264VideoStreamSettings extends VideoStreamSettings {
	var level(default,null) : String;
	var profile(default,null) : String;
	function new() : Void;
	function setProfileLevel(profile : String, level : String) : Void;
}
