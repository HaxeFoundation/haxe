package rust.type;

/** Types of this kind can be safely sent between tasks. This kind includes scalars, owning pointers, owned closures, and structural types containing only other owned types. All Owned types are Static.  */
@:native("Owned") extern interface Owned {
	
}