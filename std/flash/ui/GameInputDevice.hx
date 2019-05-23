package flash.ui;

@:require(flash11_8) extern final class GameInputDevice {
	@:flash.property var enabled(get,set) : Bool;
	@:flash.property var id(get,never) : String;
	@:flash.property var name(get,never) : String;
	@:flash.property var numControls(get,never) : Int;
	@:flash.property var sampleInterval(get,set) : Int;
	function new() : Void;
	function getCachedSamples(data : flash.utils.ByteArray, append : Bool = false) : Int;
	function getControlAt(i : Int) : GameInputControl;
	private function get_enabled() : Bool;
	private function get_id() : String;
	private function get_name() : String;
	private function get_numControls() : Int;
	private function get_sampleInterval() : Int;
	private function set_enabled(value : Bool) : Bool;
	private function set_sampleInterval(value : Int) : Int;
	function startCachingSamples(numSamples : Int, controls : flash.Vector<String>) : Void;
	function stopCachingSamples() : Void;
	static final MAX_BUFFER_SIZE : Int;
}
