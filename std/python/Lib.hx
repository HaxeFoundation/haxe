/*
 * Copyright (C)2005-2012 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package python;

import python.internal.AnonObject;
import python.Dict;
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
	@:access(python.Boot.isAnonObject)
	public static function anonToDict (o:{}):Dict<String, Dynamic> {
		return if (Boot.isAnonObject(o))
		{
			(Syntax.field(o, "__dict__"):Dict<String,Dynamic>).copy();
		}
		else null;

	}

	/**
	 	Returns the underlying Dictionary of the anonymous object `o`.
	 	Modifications to this dictionary are reflected in the anonymous Object too.
	**/
	@:access(python.Boot.isAnonObject)
	public static function anonAsDict (o:{}):Dict<String, Dynamic> {
		return if (Boot.isAnonObject(o))
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
						throw new python.Exceptions.StopIteration();
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
