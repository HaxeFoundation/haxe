package eval.luv;

@:forward
abstract RandomRequest(Request) to Request {}

/**
	System entropy source.

	@see https://aantron.github.io/luv/luv/Luv/Random
**/
extern class Random {

	static function createRequest():RandomRequest;

	/**
		Fills the given buffer with bits from the system entropy source.
	**/
	static function random(loop:Loop, buffer:Buffer, ?request:RandomRequest, callback:(result:Result<Result.NoData>)->Void):Void;
}

extern class RandomSync {
	/**
		Fills the given buffer with bits from the system entropy source.
	**/
	static function random(buffer:Buffer):Result<Result.NoData>;
}