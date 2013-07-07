package flash.display;

@:require(flash11_2) @:final extern class Worker extends flash.events.EventDispatcher {
	var running(default,null) : Bool;
	var view(default,null) : StageWorker;
	function new() : Void;
	function load(request : flash.net.URLRequest) : Void;
	function unload() : Void;
}
