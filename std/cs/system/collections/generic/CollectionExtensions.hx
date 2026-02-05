package cs.system.collections.generic;

@:native("System.Collections.Generic.CollectionExtensions")
extern class CollectionExtensions {
	@:overload(function<TKey, TValue>(dictionary:cs.system.collections.generic.IReadOnlyDictionary<TKey, TValue>, key:TKey):TValue {})
	/**
	 * @param TKey 
	 * @param TValue 
	 * @param dictionary 
	 * @param key 
	 */
	static function GetValueOrDefault<TKey, TValue>(dictionary:cs.system.collections.generic.IReadOnlyDictionary<TKey, TValue>, key:TKey, defaultValue:TValue):TValue;
	/**
	 * @param TKey 
	 * @param TValue 
	 * @param dictionary 
	 * @param key 
	 * @param value 
	 */
	static function Remove<TKey, TValue>(dictionary:cs.system.collections.generic.IDictionary<TKey, TValue>, key:TKey, value:cs.Ref<TValue>):Bool;
	/**
	 * @param TKey 
	 * @param TValue 
	 * @param dictionary 
	 * @param key 
	 * @param value 
	 */
	static function TryAdd<TKey, TValue>(dictionary:cs.system.collections.generic.IDictionary<TKey, TValue>, key:TKey, value:TValue):Bool;
}
