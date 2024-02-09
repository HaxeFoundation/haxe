package eval.luv;

/**
	Metrics.

	@see https://aantron.github.io/luv/luv/Luv/Metrics
**/
extern class Metrics {
	/**
		Retrieves the amount of time the loop has been blocked waiting in the kernel.
	**/
	static function idleTime(loop:Loop):eval.integers.UInt64;
}