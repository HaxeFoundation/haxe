package flash.display;

@:final extern class JPEGCompressOptions implements IBitmapCompressOptions {
	var quality : UInt;
	function new(quality : UInt = 80) : Void;
}
