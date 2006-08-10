package flash.net;

extern class URLLoader extends flash.events.EventDispatcher {
	function new(?request : flash.net.URLRequest) : Void;
	var bytesLoaded : UInt;
	var bytesTotal : UInt;
	function close() : Void;
	var data : Dynamic;
	var dataFormat : String;
	function load(request : flash.net.URLRequest) : Void;
	private function onComplete(event : flash.events.Event) : Void;
	private function onProgress(event : flash.events.ProgressEvent) : Void;
	private function redirectEvent(event : flash.events.Event) : Void;
	private var stream : flash.net.URLStream;
}
