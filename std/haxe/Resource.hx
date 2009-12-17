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

class Resource {
#if php
	static function cleanName(name : String) : String {
		return ~/[\\\/:?"*<>|]/.replace(name, '_');
	}

	static function getDir() {
		return untyped __call__('dirname', __php__('__FILE__'))+"/../../res";
	}

	static function getPath(name : String) {
		return getDir()+'/'+cleanName(name);
	}

	public static function listNames() : Array<String> {
		var a = php.FileSystem.readDirectory(getDir());
		if(a[0] == '.') a.shift();
		if(a[0] == '..') a.shift();
		return a;
	}

	public static function getString( name : String ) {
		return php.io.File.getContent(getPath(name));
	}

	public static function getBytes( name : String ) {
		return php.io.File.getBytes(getPath(name));
	}
#elseif cpp
	public static function listNames() : Array<String> {
		return untyped __global__.__hxcpp_resource_names();
	}
	public static function getString(name:String) : String {
		return untyped __global__.__hxcpp_resource_string(name);
	}
	public static function getBytes(name:String) : haxe.io.Bytes {
		var array:haxe.io.BytesData = untyped __global__.__hxcpp_resource_bytes(name);
		if (array==null) return null;
		return haxe.io.Bytes.ofData(array);
	}
#else
	static var content : Array<{ name : String, data : String, str : String }>;

	public static function listNames() : Array<String> {
		var names = new Array();
		for( x in content )
			names.push(x.name);
		return names;
	}

	public static function getString( name : String ) : String {
		for( x in content )
			if( x.name == name ) {
				#if neko
				return new String(x.data);
				#else
				if( x.str != null ) return x.str;
				var b : haxe.io.Bytes = haxe.Unserializer.run(x.data);
				return b.toString();
				#end
			}
		return null;
	}

	public static function getBytes( name : String ) : haxe.io.Bytes {
		for( x in content )
			if( x.name == name ) {
				#if neko
				return haxe.io.Bytes.ofData(cast x.data);
				#else
				if( x.str != null ) return haxe.io.Bytes.ofString(x.str);
				return haxe.Unserializer.run(x.data);
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
#end
}
