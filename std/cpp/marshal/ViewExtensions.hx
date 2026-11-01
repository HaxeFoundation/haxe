package cpp.marshal;

import cpp.Pointer;
import cpp.Star;
import cpp.RawPointer;
import cpp.Char;
import cpp.Char16;
import cpp.UInt8;
import cpp.NativeString;
import haxe.io.Bytes;
import haxe.io.ArrayBufferView;
import haxe.io.Float32Array;
import haxe.io.Float64Array;
import haxe.io.Int32Array;
import haxe.io.UInt32Array;
import haxe.io.UInt16Array;
import haxe.io.UInt8Array;
import haxe.ds.Vector;
import haxe.extern.AsVar;
import haxe.exceptions.ArgumentException;

final class ViewExtensions {
	public static inline overload extern function asView<T>(source:Pointer<T>, length:Int64):View<T> {
		return new View(source, length);
	}

	public static inline overload extern function asView<T>(source:Array<T>):View<T> {
		return new View(Pointer.ofArray(source), source.length);
	}

	public static inline overload extern function asView<T>(source:Vector<T>):View<T> {
		return new View(Pointer.ofArray(source.toData()), source.length);
	}

	public static inline overload extern function asView(source:Bytes):View<UInt8> {
		return new View(Pointer.ofArray(source.getData()), source.length);
	}

	public static inline overload extern function asView(source:ArrayBufferView):View<UInt8> {
		return asView(source.buffer).slice(source.byteOffset, source.byteLength);
	}

	public static inline overload extern function asView(source:Float32Array):View<cpp.Float32> {
		return asView(source.view).reinterpret();
	}

	public static inline overload extern function asView(source:Float64Array):View<cpp.Float64> {
		return asView(source.view).reinterpret();
	}

	public static inline overload extern function asView(source:Int32Array):View<cpp.Int32> {
		return asView(source.view).reinterpret();
	}

	public static inline overload extern function asView(source:UInt32Array):View<cpp.UInt32> {
		return asView(source.view).reinterpret();
	}

	public static inline overload extern function asView(source:UInt16Array):View<cpp.UInt16> {
		return asView(source.view).reinterpret();
	}

	public static inline overload extern function asView(source:UInt8Array):View<cpp.UInt8> {
		return asView(source.view).reinterpret();
	}

	public static inline extern function refAsView<T>(source:AsVar<T>):View<T> {
		return new View(Pointer.addressOf(source), 1);
	}

	public static inline extern function empty<T>():View<T> {
		return new View(null, 0);
	}

	public static inline extern function asBytesView<T>(source:View<T>):View<UInt8> {
		return source.reinterpret();
	}

	/**
	 * Allocates a new `Array` instance and copies `source` into it.
	 *
	 * @throws ArgumentException If `source` represents a region of bytes greater than the max Int32 value.
	 */
	@:unreflective @:generic public static function toArray<T>(source:View<T>):Array<T> {
		if (source.length > 2147483647) {
			throw new ArgumentException("source");
		}

		final output      = cpp.NativeArray.create(cast source.length);
		final destination = asView(output);

		source.copyTo(destination);

		return output;
	}

	/**
	 * Allocates a new `haxe.ds.Vector` instance and copies `source` into it.
	 *
	 * @throws ArgumentException If `source` represents a region of bytes greater than the max Int32 value.
	 */
	@:unreflective @:generic public static function toVector<T>(source:View<T>):Vector<T> {
		if (source.length > 2147483647) {
			throw new ArgumentException("source");
		}

		final output      = new Vector(cast source.length);
		final destination = asView(output);

		source.copyTo(destination);

		return output;
	}

	/**
	 * Allocates a new `haxe.io.Bytes` instance and copies `source` into it.
	 *
	 * @throws ArgumentException If `source` represents a region of bytes greater than the max Int32 value.
	 */
	@:unreflective @:generic public static function toBytes<T>(source:View<T>):Bytes {
		final bytes = asBytesView(source);
		
		if (bytes.length > 2147483647) {
			throw new ArgumentException("source");
		}

		final output      = Bytes.alloc(cast bytes.length);
		final destination = ViewExtensions.asView(output);

		bytes.copyTo(destination);

		return output;
	}

	/**
	 * Reads UTF-8 characters from the view up to the first null character and decodes them into a string.
	 */
	public static inline overload extern function szToString(source:View<Char>):String {
        final bytes = asBytesView(source);

        var count = 0i64;
        while (count < bytes.length) {
			final codepoint = cpp.encoding.Utf8.codepoint(bytes.slice(count));
			
            if (0 == codepoint) {
                break;
            } else {
                count += cpp.encoding.Utf8.getByteCount(codepoint);
            }
        }

        return cpp.encoding.Utf8.decode(bytes.slice(0, count));
	}

	/**
	 * Reads UTF-16 characters from the view up to the first null character and decodes them into a string.
	 */
	public static inline overload extern function szToString(source:View<Char16>):String {
        final bytes = asBytesView(source);

        var count = 0i64;
        while (count < bytes.length) {
			final codepoint = cpp.encoding.Utf16.codepoint(bytes.slice(count));
			
            if (0 == codepoint) {
                break;
            } else {
                count += cpp.encoding.Utf16.getByteCount(codepoint);
            }
        }

        return cpp.encoding.Utf16.decode(bytes.slice(0, count));
	}
}