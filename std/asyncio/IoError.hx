package asyncio;

import asyncio.IoErrorType;
import haxe.Exception;
import haxe.PosInfos;

class IoError extends Exception {
	/**
		Error type
	**/
	public final type:IoErrorType;

	public function new(type:IoErrorType, ?previous:Exception) {
		super(type.toString(), previous);
		this.type = type;
	}
}