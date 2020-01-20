package aio;

import aio.IoErrorType;
import haxe.Error;
import haxe.PosInfos;

class IoError extends Error {
	/**
		Error number
	**/
	public final type:IoErrorType;

	public function new(type:IoErrorType, ?pos:PosInfos) {
		super(type.toString(), pos);
		this.type = type;
	}
}