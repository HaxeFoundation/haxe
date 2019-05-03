package flash.media;

extern final class AudioDeviceManager extends flash.events.EventDispatcher {
	var deviceNames(get,never) : Array<Dynamic>;
	var selectedDeviceIndex(get,set) : Int;
	function new() : Void;
	private function get_deviceNames() : Array<Dynamic>;
	private function get_selectedDeviceIndex() : Int;
	private function set_selectedDeviceIndex(value : Int) : Int;
	static var audioDeviceManager(get,never) : AudioDeviceManager;
	static var isSupported(get,never) : Bool;
	private static function get_audioDeviceManager() : AudioDeviceManager;
	private static function get_isSupported() : Bool;
}
