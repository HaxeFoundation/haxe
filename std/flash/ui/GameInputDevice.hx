package flash.ui;

@:final @:require(flash11_8) extern class GameInputDevice {
	var enabled : Bool;
	var id(default,never) : String;
	var name(default,never) : String;
	var numControls(default,never) : Int;
	var sampleInterval : Int;
	function new() : Void;
	function getCachedSamples(data : flash.utils.ByteArray, append : Bool = false) : Int;
	function getControlAt(i : Int) : GameInputControl;
	function startCachingSamples(numSamples : Int, controls : flash.Vector<String>) : Void;
	function stopCachingSamples() : Void;
	static var MAX_BUFFER_SIZE(default,never) : Int;
}
