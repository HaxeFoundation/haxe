package python;

import python.lib.Types;

typedef PySys = python.lib.Sys;
typedef PyStringTools = python.lib.StringTools;

class Lib {

	public static function print(v:Dynamic):Void {
		var str = Std.string(v);

		PySys.stdout.buffer.write( PyStringTools.encode(str, "utf-8"));
		PySys.stdout.flush();
	}

	public static function println(v:Dynamic):Void {
		var str = Std.string(v);

		PySys.stdout.buffer.write( PyStringTools.encode('$str\n', "utf-8"));
		PySys.stdout.flush();
	}

	public static function toPythonIterable <T>(it:Iterable<T>):python.lib.Types.NativeIterable<T> {
		return {
			__iter__ : function () {
				var it1 = it.iterator();
				var self:PyIterator<T> = null;
				self = new PyIterator({
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
