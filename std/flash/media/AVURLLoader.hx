package flash.media;

extern class AVURLLoader extends flash.net.URLLoader {
	var cookieHeader(null,default) : String;
	var stream : AVURLStream;
	function new(?request : flash.net.URLRequest) : Void;
}
