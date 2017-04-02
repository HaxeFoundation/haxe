/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package haxe.ds;

@:headerClassCode("
  inline void set(int key, ::null value) { __int_hash_set(h,key,value); }
  inline void set(int key, bool value) { __int_hash_set(h,key,value); }
  inline void set(int key, char value) { __int_hash_set_int(h,key,value); }
  inline void set(int key, unsigned char value) { __int_hash_set_int(h,key,value); }
  inline void set(int key, signed char value) { __int_hash_set_int(h,key,value); }
  inline void set(int key, short value) { __int_hash_set_int(h,key,value); }
  inline void set(int key, unsigned short value) { __int_hash_set_int(h,key,value); }
  inline void set(int key, int value) { __int_hash_set_int(h,key,value); }
  inline void set(int key, unsigned int value) { __int_hash_set_int(h,key,value); }
  inline void set(int key, float value) { __int_hash_set_float(h,key,value); }
  inline void set(int key, double value) { __int_hash_set_float(h,key,value); }
  inline void set(int key, ::String value) { __int_hash_set_string(h,key,value); }

  template<typename V, typename H>
  inline void set(int key, const ::cpp::Struct<V,H> &value) {__int_hash_set(h,key,value); }
  template<typename F>
  inline void set(int key, const ::cpp::Function<F> &value) {__int_hash_set(h,key,value); }
  template<typename V>
  inline void set(int key, const ::cpp::Pointer<V> &value) {__int_hash_set(h,key,(Dynamic)value ); }

  template<typename VALUE>
  inline void set(Dynamic &key, const VALUE &value) { set( (int)key, value ); }
")
@:coreApi class IntMap<T> implements haxe.Constraints.IMap<Int,T> {

	@:ifFeature("haxe.ds.IntMap.*")
	private var h : Dynamic;

	public function new() : Void { }

	public function set( key : Int, value : T ) : Void {
		untyped __global__.__int_hash_set(h,key,value);
	}

	public function get( key : Int ) : Null<T> {
		return untyped __global__.__int_hash_get(h,key);
	}

	public function exists( key : Int ) : Bool {
		return untyped __global__.__int_hash_exists(h,key);
	}

	public function remove( key : Int ) : Bool {
		return untyped __global__.__int_hash_remove(h,key);
	}

	public function keys() : Iterator<Int> {
		var a:Array<Int> = untyped __global__.__int_hash_keys(h);
		return a.iterator();
	}

	public function iterator() : Iterator<T> {
		var a:Array<Dynamic> = untyped __global__.__int_hash_values(h);
		return a.iterator();
	}

	public function toString() : String {
		return untyped __global__.__int_hash_to_string(h);
	}
}
