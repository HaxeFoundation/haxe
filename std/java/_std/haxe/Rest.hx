package haxe;

import haxe.iterators.RestIterator;
import haxe.iterators.RestKeyValueIterator;
import java.NativeArray;
import java.lang.System;
import java.lang.Object;
import java.util.Arrays;
import java.StdTypes;

private typedef NativeRest<T> = NativeArray<T>;

@:coreApi
abstract Rest<T>(NativeRest<T>) {
	public var length(get,never):Int;
	inline function get_length():Int
		return this.length;

	#if jvm
	@:from static public function of<T>(array:Array<T>):Rest<T> {
		return new Rest(@:privateAccess array.__a);
	}
	#else
	@:from extern inline static public function of<T>(array:Array<T>):Rest<T> {
		var result = createNative(array.length);
		var src:NativeArray<Object> = cast @:privateAccess array.__a;
		for(i in 0...src.length)
			result[i] = cast src[i];
		return new Rest(result);
	}
	#end

	@:noDoc
	@:generic
	static function ofNativePrimitive<TBoxed,TRest>(result:NativeRest<TBoxed>, collection:NativeArray<TRest>):Rest<TRest> {
		for(i in 0...collection.length)
			result[i] = cast collection[i];
		return new Rest(cast result);
	}

	@:noDoc
	@:from static function ofNativeInt(collection:NativeArray<Int>):Rest<Int>
		return ofNativePrimitive(new NativeRest<java.lang.Integer>(collection.length), collection);

	@:noDoc
	@:from static function ofNativeFloat(collection:NativeArray<Float>):Rest<Float>
		return ofNativePrimitive(new NativeRest<java.lang.Double>(collection.length), collection);

	@:noDoc
	@:from static function ofNativeBool(collection:NativeArray<Bool>):Rest<Bool>
		return ofNativePrimitive(new NativeRest<java.lang.Boolean>(collection.length), collection);

	@:noDoc
	@:from static function ofNativeInt8(collection:NativeArray<Int8>):Rest<Int8>
		return ofNativePrimitive(new NativeRest<java.lang.Byte>(collection.length), collection);

	@:noDoc
	@:from static function ofNativeInt16(collection:NativeArray<Int16>):Rest<Int16>
		return ofNativePrimitive(new NativeRest<java.lang.Short>(collection.length), collection);

	@:noDoc
	@:from static function ofNativeChar16(collection:NativeArray<Char16>):Rest<Char16>
		return ofNativePrimitive(new NativeRest<java.lang.Character>(collection.length), collection);

	@:noDoc
	@:from static function ofNativeHaxeInt64(collection:NativeArray<haxe.Int64>):Rest<haxe.Int64>
		return ofNativePrimitive(new NativeRest<java.lang.Long>(collection.length), collection);

	@:noDoc
	@:from static function ofNativeInt64(collection:NativeArray<Int64>):Rest<Int64>
		return ofNativePrimitive(new NativeRest<java.lang.Long>(collection.length), collection);

	@:noDoc
	@:from static function ofNative<T>(collection:NativeArray<T>):Rest<T> {
		return new Rest(collection);
	}

	inline function new(a:NativeRest<T>):Void
		this = a;

	/**
	 * Implemented in genjvm (to auto-box primitive types) and genjava
	 */
	static function createNative<T>(length:Int):NativeRest<T>
		return new NativeRest<T>(length);

	@:arrayAccess inline function get(index:Int):T
		return this[index];

	@:to public function toArray():Array<T> {
		return [for(i in 0...this.length) this[i]];
	}

	public inline function iterator():RestIterator<T>
		return new RestIterator<T>(this);

	public inline function keyValueIterator():RestKeyValueIterator<T>
		return new RestKeyValueIterator<T>(this);

	extern inline public function append(item:T):Rest<T> {
		return _append(createNative(this.length + 1), item);
	}
	function _append(result:NativeRest<T>, item:T):Rest<T> {
		System.arraycopy(this, 0, result, 0, this.length);
		result[this.length] = cast item;
		return new Rest(result);
	}

	extern inline public function prepend(item:T):Rest<T> {
		return _prepend(createNative(this.length + 1), item);
	}
	function _prepend(result:NativeRest<T>, item:T):Rest<T> {
		System.arraycopy(this, 0, result, 1, this.length);
		result[0] = cast item;
		return new Rest(result);
	}

	public function toString():String {
		return toArray().toString();
	}
}