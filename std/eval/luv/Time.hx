package eval.luv;

import eval.integers.Int64;
import eval.integers.UInt64;

/**
	Current time.

	@see https://aantron.github.io/luv/luv/Luv/Time
**/
extern class Time {
	/**
		Get time.
	**/
	static function getTimeOfDay():Result<{sec:Int64, usec:Int}>;

	/**
		Samples the high-resolution timer.
	**/
	static function hrTime():UInt64;

	/**
		Suspends the calling thread for at least the given number of milliseconds.
	**/
	static function sleep(duration:Int):Void;

}