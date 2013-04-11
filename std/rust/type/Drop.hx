package rust.type;

/** This is not strictly a kind, but its presence interacts with kinds: the Drop trait provides a single method finalize that takes no parameters, and is run when values of the type are dropped. Such a method is called a "destructor", and are always executed in "top-down" order: a value is completely destroyed before any of the values it owns run their destructors. Only Owned types that do not implement Copy can implement Drop. */
@:native("Drop") extern interface Drop {
	
}