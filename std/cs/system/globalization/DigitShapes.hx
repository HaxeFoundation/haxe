package cs.system.globalization;

/** Specifies the culture-specific display of digits. */
@:native("System.Globalization.DigitShapes")
extern enum abstract DigitShapes(Int) {
	var Context = 0;
	var NativeNational = 2;
	var None = 1;
}
