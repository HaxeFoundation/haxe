package eval.luv;

/**
	Barriers.

	@see https://aantron.github.io/luv/luv/Luv/Barrier
**/
@:coreType abstract Barrier {
	/**
		Allocates and initializes a barrier.
	**/
	static public function init(count:Int):Result<Barrier>;

	/**
		Cleans up a barrier.
	**/
	public function destroy():Void;

	/**
		Waits on a barrier.
	**/
	public function wait():Bool;
}