package cs.system;

/** Specifies how mathematical rounding methods should process a number that is midway between two numbers. */
@:native("System.MidpointRounding")
extern enum MidpointRounding {
	AwayFromZero;
	ToEven;
}
