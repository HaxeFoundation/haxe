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
package hl;

@:enum
abstract TypeKind(Int) {
	public var HVoid = 0;
	public var HUI8 = 1;
	public var HUI16 = 2;
	public var HI32 = 3;
	public var HF32 = 4;
	public var HF64 = 5;
	public var HBool = 6;
	public var HBytes = 7;
	public var HDyn = 8;
	public var HFun = 9;
	public var HObj = 10;
	public var HArray = 11;
	public var HType = 12;
	public var HRef = 13;
	public var HVirtual = 14;
	public var HDynObj = 15;
	public var HAbstract = 16;
	public var HEnum = 17;
	public var HNull = 18;
}

@:coreType abstract Type {

	public var kind(get,never) : TypeKind;

	@:extern inline function get_kind() : TypeKind {
		return untyped $tkind(this);
	}

	@:hlNative("std","type_name") function getNameBytes() : Bytes {
		return null;
	}

	@:extern public static inline function getDynamic( v : Dynamic ) : Type {
		return untyped $tdyntype(v);
	}

	@:extern public static inline function get<T>( v : T ) : Type {
		return untyped $ttype(v);
	}

	@:extern public inline function getName() : String {
		var s = getNameBytes();
		return @:privateAccess String.fromUCS2(s);
	}

	@:hlNative("std", "type_safe_cast") public function safeCast( t : Type ) : Bool {
		return false;
	}

	@:hlNative("std","type_instance_fields") public function getInstanceFields() : NativeArray<Bytes> {
		return null;
	}

	@:hlNative("std","type_get_global") public function getGlobal() : Dynamic {
		return null;
	}

	@:hlNative("std","type_set_global") public function setGlobal( v : Dynamic ) : Bool {
		return false;
	}

	@:hlNative("std","type_args_count") public function getArgsCount() : Int {
		return 0;
	}

	@:hlNative("std","type_super") public function getSuper() : Type {
		return null;
	}

	@:hlNative("std","type_enum_fields") public function getEnumFields() : NativeArray<Bytes> {
		return null;
	}

	@:hlNative("std","type_enum_values") public function getEnumValues() : NativeArray<Dynamic> {
		return null;
	}

	@:hlNative("std","alloc_obj") public function allocObject() : Dynamic {
		return null;
	}

	@:hlNative("std", "alloc_enum") public function allocEnum( index : Int, args : NativeArray<Dynamic>, nargs : Int ) : Dynamic {
		return null;
	}

}