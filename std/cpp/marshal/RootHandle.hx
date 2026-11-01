package cpp.marshal;

/**
 * An object which has been rooted will not be collected by the GC even if it has no other references.
 * Rooted objects can also be used to safely pass managed objects into unmanaged code.
 */
@:semantics(value)
@:cpp.ValueType({ namespace : [ "cpp", "marshal" ] })
extern class RootHandle {
	/**
	 * Root the provided object and return a handle representing it.
	 */
	public static function create(obj:Dynamic):RootHandle;

	/**
	 * Reinterpret the provided `void*` as a rooted object.
	 * If `ptr` is not a rooted object the behaviour is undefined.
	 */
	public static function fromVoidPointer(ptr:cpp.Pointer<cpp.Void>):RootHandle;

	/**
	 * Unroot the object represented by this handle.
	 * If there are no other references to the object it will be eligible for collection by the GC.
	 */
	function close():Void;

	/**
	 * Return the object represented by this handle.
	 */
	function getObject():Dynamic;

	/**
	 * Returns the rooted object reinterpreted as a `void*`.
	 */
	function toVoidPointer():cpp.Pointer<cpp.Void>;
}
