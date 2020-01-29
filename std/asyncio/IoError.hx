package asyncio;

import asyncio.IoErrorType;
import haxe.Error;
import haxe.PosInfos;

class IoError extends Error {
	/**
		Error type
	**/
	public final type:IoErrorType;

	public function new(type:IoErrorType, ?pos:PosInfos) {
		super(type.toString(), pos);
		this.type = type;
	}
}