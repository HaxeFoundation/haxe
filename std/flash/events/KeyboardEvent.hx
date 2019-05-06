package flash.events;

extern class KeyboardEvent extends Event {
	@:flash.property var altKey(get,set) : Bool;
	@:flash.property var charCode(get,set) : UInt;
	@:flash.property var ctrlKey(get,set) : Bool;
	@:flash.property var keyCode(get,set) : UInt;
	@:flash.property var keyLocation(get,set) : flash.ui.KeyLocation;
	@:flash.property var shiftKey(get,set) : Bool;
	function new(type : String, bubbles : Bool = true, cancelable : Bool = false, charCodeValue : UInt = 0, keyCodeValue : UInt = 0, keyLocationValue : flash.ui.KeyLocation = 0, ctrlKeyValue : Bool = false, altKeyValue : Bool = false, shiftKeyValue : Bool = false) : Void;
	private function get_altKey() : Bool;
	private function get_charCode() : UInt;
	private function get_ctrlKey() : Bool;
	private function get_keyCode() : UInt;
	private function get_keyLocation() : flash.ui.KeyLocation;
	private function get_shiftKey() : Bool;
	private function set_altKey(value : Bool) : Bool;
	private function set_charCode(value : UInt) : UInt;
	private function set_ctrlKey(value : Bool) : Bool;
	private function set_keyCode(value : UInt) : UInt;
	private function set_keyLocation(value : flash.ui.KeyLocation) : flash.ui.KeyLocation;
	private function set_shiftKey(value : Bool) : Bool;
	function updateAfterEvent() : Void;
	static final KEY_DOWN : String;
	static final KEY_UP : String;
}
