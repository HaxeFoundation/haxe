package rust.type;

/** Types with destructors, closure environments, and various other non-first-class types, are not copyable at all. Such types can usually only be accessed through pointers, or in some cases, moved between mutable locations. */
@:native("Default") extern interface Default {
	
}