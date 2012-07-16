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
	
	#if (java || cs)
	static var content : Array<String>;
	#else
	static var content : Array<{ name : String, data : String, str : String }>;
	#end
	
	#if cs
	static var paths : Hash<String>;
	
	private static function getPaths():Hash<String>
	{
		if (paths != null)
			return paths;
		var p = new Hash();
		var all:cs.NativeArray<String> = untyped __cs__("typeof(haxe.Resource).Assembly.GetManifestResourceNames()");
		for (i in 0...all.Length)
		{
			var path = all[i];
			var name = path.substr(path.indexOf("Resources.") + 10);
			p.set(name, path);
		}
		
		return paths = p;
	}
	#end

	public static function listNames() : Array<String> {
		var names = new Array();
		#if (java || cs)
		for ( x in content )
			names.push(x);
		#else
		for ( x in content )
			names.push(x.name);
		#end
		return names;
	}

	public static function getString( name : String ) : String {
		#if java
		var stream = java.Lib.toNativeType(Resource).getResourceAsStream("/" + name);
		if (stream == null)
			return null;
		var stream = new java.io.NativeInput(stream);
		return stream.readAll().toString();
		#elseif cs
		var str:cs.system.io.Stream = untyped __cs__("typeof(haxe.Resource).Assembly.GetManifestResourceStream((string)getPaths().get(name).@value)");
		if (str != null)
			return new cs.io.NativeInput(str).readAll().toString();
		return null;
		#else
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
		#end
	}

	public static function getBytes( name : String ) : haxe.io.Bytes {
		#if java
		var stream = java.Lib.toNativeType(Resource).getResourceAsStream("/" + name);
		if (stream == null)
			return null;
		var stream = new java.io.NativeInput(stream);
		return stream.readAll();
		#elseif cs
		var str:cs.system.io.Stream = untyped __cs__("typeof(haxe.Resource).Assembly.GetManifestResourceStream((string)getPaths().get(name).@value)");
		if (str != null)
			return new cs.io.NativeInput(str).readAll();
		return null;
		#else
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
		#end
	}

	static function __init__() {
		#if neko
		var tmp = untyped __resources__();
		content = untyped Array.new1(tmp,__dollar__asize(tmp));
		#elseif php
		content = null;
		#elseif as3
		null;
		#elseif (java || cs)
		//do nothing
		#else
		content = untyped __resources__();
		#end
	}

}
