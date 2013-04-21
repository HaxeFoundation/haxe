package flash.ui;

@:final @:require(flash11_2) extern class GameInputDevice {
	var enabled : Bool;
	var id(default,null) : String;
	var name(default,null) : String;
	var numControls(default,null) : Int;
	var sampleInterval : Int;
	function new() : Void;
	function getCachedSamples(data : flash.utils.ByteArray, append : Bool = false) : Int;
	function getControlAt(i : Int) : GameInputControl;
	function startCachingSamples(numSamples : Int, controls : flash.Vector<String>) : Void;
	function stopCachingSamples() : Void;
	static var MAX_BUFFER_SIZE : Int;
}
