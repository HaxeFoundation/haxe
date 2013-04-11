package rust.type;

/** Types of this kind do not contain any borrowed pointers; this can be a useful guarantee for code that breaks borrowing assumptions using unsafe operations. */
@:native("Static") extern interface Static {
	
}