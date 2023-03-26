package haxe.exceptions;

/**
	An exception that carry position information of a place where it was created.
**/
class PosException extends Exception {
	/**
		Position where this exception was created.
	**/
	public final posInfos:PosInfos;

	public function new(message:String, ?previous:Exception, ?pos:PosInfos):Void {
		super(message, previous);
		if (pos == null) {
			posInfos = { fileName:'(unknown)', lineNumber:0, className:'(unknown)', methodName:'(unknown)' }
		} else {
			posInfos = pos;
		}
	}

	/**
		Returns exception message.
	**/
	override function toString():String {
		return '${super.toString()} in ${posInfos.className}.${posInfos.methodName} at ${posInfos.fileName}:${posInfos.lineNumber}';
	}
}