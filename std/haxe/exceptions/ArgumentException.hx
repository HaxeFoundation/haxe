package haxe.exceptions;

/**
	An exception that is thrown when an invalid value provided for an argument of a function.
**/
class ArgumentException extends PosException {
	/**
		An argument name.
	**/
	public final argument:String;

	public function new(argument:String, ?message:String, ?previous:Exception, ?pos:PosInfos):Void {
		super(message == null ? 'Invalid argument "$argument"' : message, previous, pos);
		this.argument = argument;
	}
}