package haxe.exceptions;

/**
	An exception that is thrown when requested function or operation is
	not supported or cannot be implemented.
**/
class NotSupportedException extends Exception {
	/**
		Returns an instance of `NotSupportedException` with the message telling
		that the caller of this method is not supported on current platform.
	**/
	static public function field(?pos:PosInfos):NotSupportedException {
		return new NotSupportedException('${@:privateAccess PosException.fieldPath(pos)} is not supported on this platform');
	}

	public function new(message:String = 'Operation not supported', ?previous:Exception):Void {
		super(message, previous);
	}
}