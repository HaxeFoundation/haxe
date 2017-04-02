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

/**
	Resource can be used to access resources that were added through the
	-resource file@name command line parameter.

	Depending on their type they can be obtained as String through
	getString(name), or as binary data through getBytes(name).

	A list of all available resource names can be obtained from listNames().
**/
class Resource {

	static var content : Array<{ name : String, data : String, str : String }>;

	/**
		Lists all available resource names. The resource name is the name part
		of the -resource file@name command line parameter.
	**/
	public static function listNames() : Array<String> {
		return [for (x in content) x.name];
	}

	/**
		Retrieves the resource identified by `name` as a String.

		If `name` does not match any resource name, null is returned.
	**/
	public static function getString( name : String ) : String {
		for( x in content )
			if( x.name == name ) {
				#if neko
				return new String(x.data);
				#else
				if( x.str != null ) return x.str;
				var b : haxe.io.Bytes = haxe.crypto.Base64.decode(x.data);
				return b.toString();
				#end
			}
		return null;
	}

	/**
		Retrieves the resource identified by `name` as an instance of
		haxe.io.Bytes.

		If `name` does not match any resource name, null is returned.
	**/
	public static function getBytes( name : String ) : haxe.io.Bytes {
		for( x in content )
			if( x.name == name ) {
				#if neko
				return haxe.io.Bytes.ofData(cast x.data);
				#else
				if( x.str != null ) return haxe.io.Bytes.ofString(x.str);
				return haxe.crypto.Base64.decode(x.data);
				#end
			}
		return null;
	}

	static function __init__() {
		#if neko
		var tmp = untyped __resources__();
		content = untyped Array.new1(tmp,__dollar__asize(tmp));
		#elseif php
		content = null;
		#elseif as3
		null;
		#else
		content = untyped __resources__();
		#end
	}

}
