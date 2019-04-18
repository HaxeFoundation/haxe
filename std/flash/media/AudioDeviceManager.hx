package flash.media;

extern final class AudioDeviceManager extends flash.events.EventDispatcher {
	var deviceNames(default,never) : Array<Dynamic>;
	var selectedDeviceIndex : Int;
	function new() : Void;
	static var audioDeviceManager(default,never) : AudioDeviceManager;
	static var isSupported(default,never) : Bool;
}
