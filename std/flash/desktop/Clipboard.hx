package flash.desktop;

@:require(flash10) extern class Clipboard {
	@:flash.property var formats(get,never) : Array<ClipboardFormats>;
	function clear() : Void;
	function clearData(format : ClipboardFormats) : Void;
	function getData(format : ClipboardFormats, ?transferMode : ClipboardTransferMode) : flash.utils.Object;
	private function get_formats() : Array<ClipboardFormats>;
	function hasFormat(format : ClipboardFormats) : Bool;
	function setData(format : ClipboardFormats, data : flash.utils.Object, serializable : Bool = true) : Bool;
	function setDataHandler(format : ClipboardFormats, handler : flash.utils.Function, serializable : Bool = true) : Bool;
	@:flash.property static var generalClipboard(get,never) : Clipboard;
	private static function get_generalClipboard() : Clipboard;
}
