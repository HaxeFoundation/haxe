/*
 * Copyright (c) 2005-2008, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package haxe;

#if as3
@:coreApi
class Resource {
	public static function listNames() : Array<String> untyped {
		return __keys__(__resources__.list);
	}

	public static function getString( name : String ) : String {
		var b = resolve(name);
		return b == null ? null : b.readUTFBytes(b.length);
	}

	public static function getBytes( name : String ) : haxe.io.Bytes {
		var b = resolve(name);
		return b == null ? null : haxe.io.Bytes.ofData(b);
	}

	static function resolve( name : String) :flash.utils.ByteArray untyped {
		var n = __resources__.list[name];
		if (n == null) return null;
		return untyped __new__(n);
	}

	static function __init__() : Void {
		untyped __resources__.__init__();
	}
}
#else
@:coreApi
class Resource {

	static var content : Array<{ name : String }>;

	public static function listNames() : Array<String> {
		var names = new Array();
		for( x in content )
			names.push(x.name);
		return names;
	}

	public static function getString( name : String ) : String {
		var b = resolve(name);
		return b == null ? null : b.readUTFBytes(b.length);
	}

	public static function getBytes( name : String ) : haxe.io.Bytes {
		var b = resolve(name);
		return b == null ? null : haxe.io.Bytes.ofData(b);
	}

	static function resolve( name : String ) : flash.utils.ByteArray {
		try untyped {
			var c = __as__(__global__["flash.utils.getDefinitionByName"]("_res._"+name.split(".").join("_")),Class);
			return __new__(c);
		} catch( e : Dynamic ) {
			return null;
		}
	}

	static function __init__() : Void {
		content = untyped __resources__();
	}
}
#end