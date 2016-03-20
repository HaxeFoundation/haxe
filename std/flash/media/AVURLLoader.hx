package flash.media;

extern class AVURLLoader extends flash.net.URLLoader {
	var cookieHeader(never,default) : String;
	var stream : AVURLStream;
	function new(?request : flash.net.URLRequest) : Void;
}
