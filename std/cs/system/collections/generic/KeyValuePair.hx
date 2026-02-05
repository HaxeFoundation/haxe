package cs.system.collections.generic;

@:native("System.Collections.Generic.KeyValuePair")
extern class KeyValuePair {
	/**
	 * @param TKey 
	 * @param TValue 
	 * @param key 
	 * @param value 
	 */
	static function Create<TKey, TValue>(key:TKey, value:TValue):cs.system.collections.generic.KeyValuePair_2<TKey, TValue>;
}
