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

	public static function dictToAnon (v:Dict<String, Dynamic>):Dynamic
	{
		return new AnonObject(v.copy());
	}


	public static function anonToDict (o:{}):Dict<String, Dynamic>
	{
		return if (python.lib.Builtin.isinstance(o, AnonObject))
		{
			(Syntax.field(o, "__dict__"):Dict<String,Dynamic>).copy();
		}
		else null;

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
