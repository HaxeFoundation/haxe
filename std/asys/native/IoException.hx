package asys.native;

import asys.native.IoErrorType;
import haxe.Exception;

class IoException extends Exception {
	/**
		Error type
	**/
	public final type:IoErrorType;

	public function new(type:IoErrorType, ?previous:Exception) {
		super(type.toString(), previous);
		this.type = type;
	}
}