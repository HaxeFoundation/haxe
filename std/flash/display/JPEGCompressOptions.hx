package flash.display;

extern final class JPEGCompressOptions implements IBitmapCompressOptions {
	var quality : UInt;
	function new(quality : UInt = 80) : Void;
}
