package flash.desktop;

@:native("flash.desktop.ClipboardTransferMode") @:require(flash10) extern enum abstract ClipboardTransferMode(String) {
	var CLONE_ONLY;
	var CLONE_PREFERRED;
	var ORIGINAL_ONLY;
	var ORIGINAL_PREFERRED;
}
