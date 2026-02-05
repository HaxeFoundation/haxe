package cs.system.collections;

/** Supplies a hash code for an object, using a custom hash function. */
@:native("System.Collections.IHashCodeProvider")
extern interface IHashCodeProvider {
	/**
	 * Returns a hash code for the specified object.
	 * @param obj The  for which a hash code is to be returned.
	 * @return A hash code for the specified object.
	 */
	function GetHashCode(obj:Dynamic):Int;
}
