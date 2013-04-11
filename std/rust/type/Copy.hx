package rust.type;
/** This kind includes all types that can be copied. All types with sendable kind are copyable, as are managed boxes, managed closures, trait types, and structural types built out of these. Types with destructors (types that implement Drop) can not implement Copy. */
@:native("Copy") extern interface Copy {
	
}