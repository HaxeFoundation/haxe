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
package haxe;

private class ResourceContent {
	public var name : hl.Bytes;
	public var data : hl.Bytes;
	public var dataLen : Int;
}

@:coreApi
class Resource {

	static var content : hl.NativeArray<ResourceContent>;

	public static function listNames() : Array<String> {
		return [for (x in content) @:privateAccess String.fromUCS2(x.name)];
	}

	public static function getString( name : String ) : String {
		for( x in content )
			if( x.name.compare(0,@:privateAccess name.bytes,0,(name.length+1)<<1) == 0 )
				return @:privateAccess String.fromUTF8(x.data);
		return null;
	}

	public static function getBytes( name : String ) : haxe.io.Bytes {
		for( x in content )
			if( x.name.compare(0,@:privateAccess name.bytes,0,(name.length+1)<<1) == 0 )
				return @:privateAccess new haxe.io.Bytes(x.data, x.dataLen);
		return null;
	}

	static function __init__() : Void {
		content = untyped $resources();
	}

}
