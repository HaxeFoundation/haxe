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
#else
	static var content : Array<{ name : String, data : #if (neko || flash9) String #else Array<String> #end }>;

	public static function listNames() : Array<String> {
		var names = new Array();
		for( x in content )
			names.push(x.name);
		return names;
	}

	public static function getString( name : String ) {
		for( x in content )
			if( x.name == name )
				return #if neko new String(x.data) #elseif flash9 x.data #else x.data[0] #end;
		return null;
	}

	public static function getBytes( name : String ) {
		for( x in content )
			if( x.name == name ) {
				#if neko
				return haxe.io.Bytes.ofData(cast x.data);
				#elseif flash9
				var b = new flash.utils.ByteArray();
				b.writeUTFBytes(x.data);
				return haxe.io.Bytes.ofData(b);
				#else
				var buf = new haxe.io.BytesOutput();
				var first = true;
				for( seg in x.data ) {
					if( first ) first = false else buf.writeByte(0);
					buf.writeString(seg);
				}
				return buf.getBytes();
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
		#else
		content = untyped __resources__();
		#end
	}
#end
}
