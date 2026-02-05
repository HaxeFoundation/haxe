package cs.system;

/** Specifies the culture, case, and sort rules to be used by certain overloads of the  and  methods. */
@:native("System.StringComparison")
extern enum StringComparison {
	CurrentCulture;
	CurrentCultureIgnoreCase;
	InvariantCulture;
	InvariantCultureIgnoreCase;
	Ordinal;
	OrdinalIgnoreCase;
}
