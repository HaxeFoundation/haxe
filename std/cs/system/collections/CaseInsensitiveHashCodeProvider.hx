package cs.system.collections;

/** Supplies a hash code for an object, using a hashing algorithm that ignores the case of strings. */
@:native("System.Collections.CaseInsensitiveHashCodeProvider")
extern class CaseInsensitiveHashCodeProvider {
	/**
	 * Gets an instance of  that is associated with the  of the current thread and that
	 * is always available.
	 * @return An instance of  that is associated with the  of the current thread.
	 */
	static var Default(default, never):cs.system.collections.CaseInsensitiveHashCodeProvider;
	/**
	 * Gets an instance of  that is associated with  and that is always available.
	 * @return An instance of  that is associated with .
	 */
	static var DefaultInvariant(default, never):cs.system.collections.CaseInsensitiveHashCodeProvider;
	@:overload(function():Void {})
	function new(culture:cs.system.globalization.CultureInfo):Void;
	/**
	 * Returns a hash code for the given object, using a hashing algorithm that ignores
	 * the case of strings.
	 * @param obj The  for which a hash code is to be returned.
	 * @return A hash code for the given object, using a hashing algorithm that ignores
	 * the case of strings.
	 */
	function GetHashCode(obj:Dynamic):Int;
}
