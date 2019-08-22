package flash.desktop;

@:native("flash.desktop.ClipboardFormats") @:require(flash10) extern enum abstract ClipboardFormats(String) {
	var AIR_PREFIX;
	var BITMAP_FORMAT;
	var FILE_LIST_FORMAT;
	var FILE_PROMISE_LIST_FORMAT;
	var FLASH_PREFIX;
	var HTML_FORMAT;
	var REFERENCE_PREFIX;
	var RICH_TEXT_FORMAT;
	var SERIALIZATION_PREFIX;
	var TEXT_FORMAT;
	var URL_FORMAT;
}
