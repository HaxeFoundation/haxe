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

@:keep
class BaseType {
	public var __type__ : Type;
	public var __meta__ : Dynamic;
	public var __implementedBy__ : NativeArray<Type>;
	public function check( v : Dynamic ) {
		var t = Type.getDynamic(v);
		if( t.kind == HVirtual ) {
			var v2 = hl.Api.getVirtualValue(v);
			if( v2 != null ) t = Type.getDynamic(v2);
		}
		if( __implementedBy__ == null ) {
			if( t.safeCast(__type__) )
				return true;
			return false;
		}
		for( i in __implementedBy__ )
			if( t.safeCast(i) )
				return true;
		return false;
	}
}

@:keep
class Class extends BaseType {
	public var __name__ : String;
	public var __constructor__ : Dynamic;
}

@:keep
class Enum extends BaseType {
	public var __ename__ : String;
	public var __emap__ : hl.types.BytesMap;
	public var __constructs__ : Array<String>;
	public var __evalues__ : NativeArray<Dynamic>;
}

@:keep
class CoreType extends Class {
}

@:keep
class CoreEnum extends Enum {
}
