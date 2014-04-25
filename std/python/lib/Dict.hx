
package python.lib;

import python.lib.Builtin;
import python.lib.Tuple;
import python.lib.Types;
import python.NativeIterator;
import python.Syntax;


extern class DictView<T> {
	public inline function iter ():NativeIterator<T>
	{
		return Builtin.iter(this);
	}
	public inline function length ():Int
	{
		return Builtin.len(this);
	}

	public inline function iterator ():Iterator<T>
	{
		return iter();
	}
}

extern class Dict <K, V>
{
	public function new ():Void;

	public inline function length ():Int
	{
		return python.lib.Builtin.len(this);
	}

	public inline function hasKey (k:K):Bool {
		return DictImpl.hasKey(this,k);
	}

	public function clear ():Void;
	public function copy ():Dict<K,V>;
	public function get (key:K, def:V):V;

	public function update (d:Dict<K,V>):Void;

	public function keys ():DictView<K>;
	public function values ():DictView<V>;
	public function items ():DictView<Tup2<K,V>>;

	public static inline function fromObject (x:{}):Dict<String,Dynamic> {
		return DictImpl.fromObject(x);
	}
	public inline function set (key:K, val:V):Void {
		DictImpl.set(this, key, val);
	}

	public inline function remove (key:K):Void
	{
		DictImpl.remove(this, key);
	}

	public inline function iterator ():Iterator<V>
	{
		return values().iter();
	}
	public function __iter__():NativeIterator<K>;

	static function __init__ ():Void
	{
		Syntax.importFromAs("builtins", "dict", "python.lib.Dict");
	}

}

class DictImpl {
	public static inline function fromObject (x:{}) {
		var d = new Dict();
		for (f in Reflect.fields(x)) {
			d.set(f, Reflect.field(x,f));
		}
		return d;
	}
	public static inline function hasKey <X>(d:Dict<X, Dynamic>, key:X) {
		return Syntax.isIn(key, d);
	}

	public static inline function remove <X>(d:Dict<X, Dynamic>, key:X) {
		Syntax.delete(python.Syntax.arrayAccess(d, key));
	}

	public static inline function set <K,V>(d:Dict<K, V>, key:K, val:V) {
		Syntax.arraySet(d, key, val);
	}
}