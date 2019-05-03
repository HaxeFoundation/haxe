package flash.media;

extern class AVURLStream extends flash.net.URLStream {
	var cookieHeader(never,set) : String;
	function new() : Void;
	private function set_cookieHeader(value : String) : String;
}
