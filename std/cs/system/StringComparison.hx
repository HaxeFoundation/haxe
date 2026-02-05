package cs.system;

/** Specifies the culture, case, and sort rules to be used by certain overloads of the  and  methods. */
@:native("System.StringComparison")
extern enum abstract StringComparison(Int) {
	var CurrentCulture = 0;
	var CurrentCultureIgnoreCase = 1;
	var InvariantCulture = 2;
	var InvariantCultureIgnoreCase = 3;
	var Ordinal = 4;
	var OrdinalIgnoreCase = 5;
}
