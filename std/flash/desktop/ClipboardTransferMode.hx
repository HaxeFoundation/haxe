package flash.desktop;

@:fakeEnum(String) @:require(flash10) extern enum ClipboardTransferMode {
	CLONE_ONLY;
	CLONE_PREFERRED;
	ORIGINAL_ONLY;
	ORIGINAL_PREFERRED;
}
