package cs.system.collections;

/** Defines a dictionary key/value pair that can be set or retrieved. */
@:native("System.Collections.DictionaryEntry")
extern class DictionaryEntry extends cs.system.ValueType {
	/**
	 * Gets or sets the key in the key/value pair.
	 * @return The key in the key/value pair.
	 */
	var Key(default, default):Dynamic;
	/**
	 * Gets or sets the value in the key/value pair.
	 * @return The value in the key/value pair.
	 */
	var Value(default, default):Dynamic;
	function new(key:Dynamic, value:Dynamic):Void;
	/**
	 * @param key 
	 * @param value 
	 */
	function Deconstruct(key:cs.Ref<Dynamic>, value:cs.Ref<Dynamic>):Void;
}
