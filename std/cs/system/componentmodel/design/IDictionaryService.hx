package cs.system.componentmodel.design;

/** Provides a basic, component site-specific, key-value pair dictionary through a service that a designer can use to store user-defined data. */
@:native("System.ComponentModel.Design.IDictionaryService")
extern interface IDictionaryService {
	/**
	 * Gets the key corresponding to the specified value.
	 * @param value The value to look up in the dictionary.
	 * @return The associated key, or  if no key exists.
	 */
	function GetKey(value:Dynamic):Dynamic;
	/**
	 * Gets the value corresponding to the specified key.
	 * @param key The key to look up the value for.
	 * @return The associated value, or  if no value exists.
	 */
	function GetValue(key:Dynamic):Dynamic;
	/**
	 * Sets the specified key-value pair.
	 * @param key An object to use as the key to associate the value with.
	 * @param value The value to store.
	 */
	function SetValue(key:Dynamic, value:Dynamic):Void;
}
