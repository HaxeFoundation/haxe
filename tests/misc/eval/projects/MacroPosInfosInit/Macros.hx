class Macros {
	// an init macro may take a trailing ?pos:haxe.PosInfos; it is filled with the
	// (synthetic) command-line position and does not break compilation
	macro static function init(?pos:haxe.PosInfos) {
		Sys.println("init pos: file=" + pos.fileName + " line=" + pos.lineNumber + " class=" + pos.className + " method=" + pos.methodName);
		return macro null;
	}
}
