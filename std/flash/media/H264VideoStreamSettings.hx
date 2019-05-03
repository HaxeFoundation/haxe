package flash.media;

extern class H264VideoStreamSettings extends VideoStreamSettings {
	var level(get,never) : String;
	var profile(get,never) : String;
	function new() : Void;
	private function get_level() : String;
	private function get_profile() : String;
	function setProfileLevel(profile : String, level : String) : Void;
}
