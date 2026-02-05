package cs.system.threading;

/** Contains constants that specify infinite time-out intervals. This class cannot be inherited. */
@:native("System.Threading.Timeout")
extern class Timeout {
	/** A constant used to specify an infinite waiting period, for threading methods that accept an  parameter. */
	static var Infinite(default, never):Int;
	/** A constant used to specify an infinite waiting period, for methods that accept a  parameter. */
	static var InfiniteTimeSpan(default, never):cs.system.TimeSpan;
}
