/*
 * Copyright (c) 2005-2010, The haXe Project Contributors
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
package haxe.macro;
import haxe.macro.Expr;

/**
	All these methods can be called for compiler configuration macros.
**/
class Compiler {

#if neko

	static var ident = ~/^[A-Za-z_][A-Za-z0-9_]*$/;
	static var path = ~/^[A-Za-z_][A-Za-z0-9_.]*$/;

	public static function removeField( className : String, field : String, ?isStatic : Bool ) {
		if( !path.match(className) ) throw "Invalid "+className;
		if( !ident.match(field) ) throw "Invalid "+field;
		untyped load("type_patch",4)(className.__s,field.__s,isStatic == true,null);
	}

	public static function setFieldType( className : String, field : String, type : String, ?isStatic : Bool ) {
		if( !path.match(className) ) throw "Invalid "+className;
		if( !ident.match((field.charAt(0) == "$") ? field.substr(1) : field) ) throw "Invalid "+field;
		untyped load("type_patch",4)(className.__s,field.__s,isStatic == true,type.__s);
	}

	public static function addMetadata( meta : String, className : String, ?field : String, ?isStatic : Bool ) {
		if( !path.match(className) ) throw "Invalid "+className;
		if( field != null && !ident.match(field) ) throw "Invalid "+field;
		untyped load("meta_patch",4)(meta.__s,className.__s,(field == null)?null:field.__s,isStatic == true);
	}

	/**
		Include for compilation all classes defined in the given package excluding the ones referenced in the ignore list.
	**/
	public static function include( pack : String, ?rec = true, ?ignore : Array<String> ) {
		for( p in Context.getClassPath() ) {
			var p = p + pack.split(".").join("/");
			if( !neko.FileSystem.exists(p) || !neko.FileSystem.isDirectory(p) )
				continue;
			for( file in neko.FileSystem.readDirectory(p) ) {
				if( StringTools.endsWith(file, ".hx") ) {
					var cl = pack + "." + file.substr(0, file.length - 3);
					if( ignore != null && Lambda.has(ignore, cl) )
						continue;
					Context.getModule(cl);
				} else if( rec && neko.FileSystem.isDirectory(p + "/" + file) )
					include(pack + "." + file, true, ignore);
			}
		}
	}

	/**
		Exclude a given class or a complete package from being generated.
	**/
	public static function exclude( pack : String, ?rec = true ) {
		Context.onGenerate(function(types) {
			for( t in types ) {
				var b : Type.BaseType, name;
				switch( t ) {
				case TInst(c, _):
					name = c.toString();
					b = c.get();
				case TEnum(e, _):
					name = e.toString();
					b = e.get();
				default: continue;
				}
				var p = b.pack.join(".");
				if( (p == pack || name == pack) || (rec && StringTools.startsWith(p, pack + ".")) )
					b.exclude();
			}
		});
	}

	/**
		Exclude classes listed in an extern file (one per line) from being generated.
	**/
	public static function excludeFile( fileName : String ) {
		fileName = Context.resolvePath(fileName);
		var f = neko.io.File.read(fileName,true);
		var classes = new Hash();
		try {
			while( true ) {
				var l = StringTools.trim(f.readLine());
				if( l == "" || !~/[A-Za-z0-9._]/.match(l) )
					continue;
				classes.set(l,true);
			}
		} catch( e : haxe.io.Eof ) {
		}
		Context.onGenerate(function(types) {
			for( t in types ) {
				switch( t ) {
				case TInst(c, _): if( classes.exists(c.toString()) ) c.get().exclude();
				case TEnum(e, _): if( classes.exists(e.toString()) ) e.get().exclude();
				default:
				}
			}
		});
	}

	/**
		Load a type patch file that can modify declared classes fields types
	**/
	public static function patchTypes( file : String ) : Void {
		var file = Context.resolvePath(file);
		var f = neko.io.File.read(file, true);
		try {
			while( true ) {
				var r = StringTools.trim(f.readLine());
				if( r == "" || r.substr(0,2) == "//" ) continue;
				if( StringTools.endsWith(r,";") ) r = r.substr(0,-1);
				if( r.charAt(0) == "-" ) {
					r = r.substr(1);
					var isStatic = StringTools.startsWith(r,"static ");
					if( isStatic ) r = r.substr(7);
					var p = r.split(".");
					var field = p.pop();
					removeField(p.join("."),field,isStatic);
					continue;
				}
				if( r.charAt(0) == "@" ) {
					var rp = r.split(" ");
					var type = rp.pop();
					var isStatic = rp[rp.length - 1] == "static";
					if( isStatic ) rp.pop();
					var meta = rp.join(" ");
					var p = type.split(".");
					var field = if( p.length > 1 && p[p.length-2].charAt(0) >= "a" ) null else p.pop();
					addMetadata(meta,p.join("."),field,isStatic);
					continue;
				}
				var rp = r.split(" : ");
				if( rp.length > 1 ) {
					r = rp.shift();
					var isStatic = StringTools.startsWith(r,"static ");
					if( isStatic ) r = r.substr(7);
					var p = r.split(".");
					var field = p.pop();
					setFieldType(p.join("."),field,rp.join(" : "),isStatic);
					continue;
				}
				throw "Invalid type patch "+r;
			}
		} catch( e : haxe.io.Eof ) {
		}
	}

	static function load( f, nargs ) : Dynamic {
		#if macro
		return neko.Lib.load("macro", f, nargs);
		#else
		return Reflect.makeVarArgs(function(_) throw "Can't be called outside of macro");
		#end
	}

#end

}