package flash.desktop;

#if !flash10
"This class is only available for flash10+"
#end

extern class Clipboard
{
	function clear():Void;

	function clearData(format:ClipboardFormats):Void;

	var formats(default,null):Array<ClipboardFormats>;

	static var generalClipboard(default,null):Clipboard;

	function getData(format:ClipboardFormats, transferMode:ClipboardTransferMode = ClipboardTransferMode.ORIGINAL_PREFERRED):Dynamic;

	function hasFormat(format:ClipboardFormats):Bool;

	function setData(format:ClipboardFormats, data:Dynamic, serializable:Bool = true):Bool;

	function setDataHandler(format:ClipboardFormats, handler:Void->String, serializable:Bool = true):Bool;
}
