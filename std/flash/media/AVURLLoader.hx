package flash.media;

extern class AVURLLoader extends flash.net.URLLoader {
	@:flash.property var cookieHeader(never,set) : String;
	var stream : AVURLStream;
	function new(?request : flash.net.URLRequest) : Void;
	private function set_cookieHeader(value : String) : String;
}
