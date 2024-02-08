package haxe.exceptions;

/**
	An exception that is thrown when requested function or operation does not have an implementation.
**/
class NotImplementedException extends PosException {
	public function new(message:String = 'Not implemented', ?previous:Exception, ?pos:PosInfos):Void {
		super(message, previous, pos);
	}
}