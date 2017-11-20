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
  inline void set(Dynamic key, ::null value) { __object_hash_set(HX_MAP_THIS,key,value,true); }
  inline void set(Dynamic key, bool value) { __object_hash_set(HX_MAP_THIS,key,value,true); }
  inline void set(Dynamic key, char value) { __object_hash_set_int(HX_MAP_THIS,key,value,true); }
  inline void set(Dynamic key, unsigned char value) { __object_hash_set_int(HX_MAP_THIS,key,value,true); }
  inline void set(Dynamic key, signed char value) { __object_hash_set_int(HX_MAP_THIS,key,value,true); }
  inline void set(Dynamic key, short value) { __object_hash_set_int(HX_MAP_THIS,key,value,true); }
  inline void set(Dynamic key, unsigned short value) { __object_hash_set_int(HX_MAP_THIS,key,value,true); }
  inline void set(Dynamic key, int value) { __object_hash_set_int(HX_MAP_THIS,key,value,true); }
  inline void set(Dynamic key, unsigned int value) { __object_hash_set_int(HX_MAP_THIS,key,value,true); }
  inline void set(Dynamic key, float value) { __object_hash_set_float(HX_MAP_THIS,key,value,true); }
  inline void set(Dynamic key, double value) { __object_hash_set_float(HX_MAP_THIS,key,value,true); }
  inline void set(Dynamic key, ::String value) { __object_hash_set_string(HX_MAP_THIS,key,value,true); }

  template<typename V, typename H>
  inline void set(Dynamic key, const ::cpp::Struct<V,H> &value) {__object_hash_set(HX_MAP_THIS,key,value,true); }
  template<typename V>
  inline void set(Dynamic key, const ::cpp::Pointer<V> &value) {__object_hash_set(HX_MAP_THIS,key,(Dynamic)value,true ); }
  template<typename V>
  inline void set(Dynamic key, const ::cpp::Function<V> &value) {__object_hash_set(HX_MAP_THIS,key,(Dynamic)value,true ); }
")
@:coreApi
class WeakMap<K:{},V> implements haxe.Constraints.IMap<K,V> {
	@:ifFeature("haxe.ds.WeakMap.*")
	private var h : Dynamic;

	public function new() : Void { }

	public function set( key : K, value : V ) : Void {
		untyped __global__.__object_hash_set(__cpp__("HX_MAP_THIS"),key,value,true);
	}

	public function get( key : K ) : Null<V> {
		return untyped __global__.__object_hash_get(h,key);
	}

	public function exists( key : K ) : Bool {
		return untyped __global__.__object_hash_exists(h,key);
	}

	public function remove( key : K ) : Bool {
		return untyped __global__.__object_hash_remove(h,key);
	}

	public function keys() : Iterator<K> {
		var a:Array<K> = untyped __global__.__object_hash_keys(h);
		return a.iterator();
	}

	public function iterator() : Iterator<V> {
		var a:Array<Dynamic> = untyped __global__.__object_hash_values(h);
		return a.iterator();
	}
	
	public function copy() : WeakMap<K,V> {
		var copied = new WeakMap();
		for(key in keys()) copied.set(key, get(key));
		return copied;
	}


	public function toString() : String {
		return untyped __global__.__object_hash_to_string(h);
	}
}
