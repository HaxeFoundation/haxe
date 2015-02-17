package python;

import python.internal.AnonObject;
import python.lib.Dict;
import python.NativeStringTools;

typedef PySys = python.lib.Sys;

class Lib {

	public static function print(v:Dynamic):Void {
		var str = Std.string(v);

		PySys.stdout.buffer.write( NativeStringTools.encode(str, "utf-8"));
		PySys.stdout.flush();
	}

	public static function println(v:Dynamic):Void {
		var str = Std.string(v);

		PySys.stdout.buffer.write( NativeStringTools.encode('$str\n', "utf-8"));
		PySys.stdout.flush();
	}

	/**
	 	Returns an anonymous Object which holds the same data as the Dictionary `v`.
	**/
	public static function dictToAnon (v:Dict<String, Dynamic>):Dynamic {
		return new AnonObject(v.copy());
	}


	/**
	 	Returns a flat copy of the underlying Dictionary of `o`.
	**/
	public static function anonToDict (o:{}):Dict<String, Dynamic> {
		return if (python.lib.Builtin.isinstance(o, AnonObject))
		{
			(Syntax.field(o, "__dict__"):Dict<String,Dynamic>).copy();
		}
		else null;

	}

	/**
	 	Returns the underlying Dictionary of the anonymous object `o`.
	 	Modifications to this dictionary are reflected in the anonymous Object too.
	**/
	public static function anonAsDict (o:{}):Dict<String, Dynamic> {
		return if (python.lib.Builtin.isinstance(o, AnonObject))
		{
			(Syntax.field(o, "__dict__"):Dict<String,Dynamic>);
		}
		else null;
	}

	/**
	 	Returns the Dictionary `d` as an anonymous Object.
	 	Modifications to the object are reflected in the Dictionary too.
	**/
	public static function dictAsAnon (d:Dict<String, Dynamic>):Dynamic {
		return new AnonObject(d);
	}

	public static function toPythonIterable <T>(it:Iterable<T>):python.NativeIterable<T> {
		return {
			__iter__ : function () {
				var it1 = it.iterator();
				var self:NativeIterator<T> = null;
				self = new NativeIterator({
					__next__ : function ():T {
					if (it1.hasNext()) {
						return it1.next();
					} else {
						throw new python.lib.Exceptions.StopIteration();
					}
				},
				__iter__ : function () return self
				});
				return self;
			}
		}
	}

	public static inline function toHaxeIterable <T>(it:NativeIterable<T>):HaxeIterable<T> {
		return new HaxeIterable(it);
	}

	public static inline function toHaxeIterator <T>(it:NativeIterator<T>):HaxeIterator<T> {
		return new HaxeIterator(it);
	}
}
