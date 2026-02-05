package cs.system;

/** Specifies how mathematical rounding methods should process a number that is midway between two numbers. */
@:native("System.MidpointRounding")
extern enum abstract MidpointRounding(Int) {
	var AwayFromZero = 1;
	var ToEven = 0;
}
