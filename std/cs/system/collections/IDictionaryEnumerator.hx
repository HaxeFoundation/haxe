package cs.system.collections;

/** Enumerates the elements of a nongeneric dictionary. */
@:native("System.Collections.IDictionaryEnumerator")
extern interface IDictionaryEnumerator extends cs.system.collections.IEnumerator {
	/**
	 * Gets both the key and the value of the current dictionary entry.
	 * @return A  containing both the key and the value of the current dictionary
	 * entry.
	 */
	var Entry(default, never):cs.system.collections.DictionaryEntry;
	/**
	 * Gets the key of the current dictionary entry.
	 * @return The key of the current element of the enumeration.
	 */
	var Key(default, never):Dynamic;
	/**
	 * Gets the value of the current dictionary entry.
	 * @return The value of the current element of the enumeration.
	 */
	var Value(default, never):Dynamic;
}
