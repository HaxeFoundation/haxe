package flash.system;

@:native("flash.system.SecurityPanel") extern enum abstract SecurityPanel(String) {
	var CAMERA;
	var DEFAULT;
	var DISPLAY;
	var LOCAL_STORAGE;
	var MICROPHONE;
	var PRIVACY;
	var SETTINGS_MANAGER;
}
