/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package haxe.macro;
import haxe.macro.Expr;

/**
	All these methods can be called for compiler configuration macros.
**/
class Compiler {

	macro static public function getDefine( key : String ) {
		return macro $v{haxe.macro.Context.definedValue(key)};
	}

#if neko

	static var ident = ~/^[A-Za-z_][A-Za-z0-9_]*$/;
	static var path = ~/^[A-Za-z_][A-Za-z0-9_.]*$/;

	public static function allowPackage( v : String ) {
		untyped load("allow_package", 1)(v.__s);
	}

	public static function define( flag : String, ?value : String ) untyped {
		var v = flag + (value == null ? "" : "=" + value);
		load("define", 1)(v.__s);
	}

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

	public static function addClassPath( path : String ) {
		untyped load("add_class_path",1)(path.__s);
	}

	public static function getOutput() : String {
		return new String(untyped load("get_output",0)());
	}

	public static function setOutput( fileOrDir : String ) {
		untyped load("set_output",1)(untyped fileOrDir.__s);
	}

	public static function getDisplayPos() : Null<{ file : String, pos : Int }> {
		var o = untyped load("get_display_pos",0)();
		if( o != null )
			o.file = new String(o.file);
		return o;
	}

	/**
		Adds a native library depending on the platform (eg : -swf-lib for Flash)
	**/
	public static function addNativeLib( name : String ) {
		untyped load("add_native_lib",1)(name.__s);
	}

	/**
		Adds an argument to be passed to the native compiler (eg : -javac-arg for Java)
	 **/
	public static function addNativeArg( argument : String )
	{
		untyped load("add_native_arg",1)(argument.__s);
	}

	/**
		Includes all modules in package `pack` in the compilation.

		In order to include single modules, their paths can be listed directly
		on command line: `haxe ... ModuleName pack.ModuleName`.

		By default `Compiler.include` will search for modules in the directories defined with `-cp`.
		If you want to specify a different set of paths to search for modules, you can use the optional
		argument `classPath`.

		@param rec If true, recursively adds all sub-packages.
		@param ignore Array of module names to ignore for inclusion.
		@param classPaths An alternative array of paths (directory names) to use to search for modules to include.
		       Note that if you pass this argument, only the specified paths will be used for inclusion.
	**/
	public static function include( pack : String, ?rec = true, ?ignore : Array<String>, ?classPaths : Array<String> ) {
		var skip = if( ignore == null ) {
			function(c) return false;
		} else {
			function(c) return Lambda.has(ignore, c);
		}
		var displayValue = Context.definedValue("display");
		if( classPaths == null ) {
			classPaths = Context.getClassPath();
			// do not force inclusion when using completion
			switch (displayValue) {
				case null:
				case "usage":
				case _: return;
			}
			// normalize class path
			for( i in 0...classPaths.length ) {
				var cp = StringTools.replace(classPaths[i], "\\", "/");
				if(StringTools.endsWith(cp, "/"))
					cp = cp.substr(0, -1);
				if( cp == "" )
					cp = ".";
				classPaths[i] = cp;
			}
		}
		var prefix = pack == '' ? '' : pack + '.';
		for( cp in classPaths ) {
			var path = pack == '' ? cp : cp + "/" + pack.split(".").join("/");
			if( !sys.FileSystem.exists(path) || !sys.FileSystem.isDirectory(path) )
				continue;
			for( file in sys.FileSystem.readDirectory(path) ) {
				if( StringTools.endsWith(file, ".hx") ) {
					var cl = prefix + file.substr(0, file.length - 3);
					if( skip(cl) )
						continue;
					Context.getModule(cl);
				} else if( rec && sys.FileSystem.isDirectory(path + "/" + file) && !skip(prefix + file) )
					include(prefix + file, true, ignore, classPaths);
			}
		}
	}

	/**
		Exclude a class or a enum without changing it to @:nativeGen.
	**/
	static function excludeBaseType( baseType : Type.BaseType ) : Void {
		if (!baseType.isExtern) {
			var meta = baseType.meta;
			if (!meta.has(":nativeGen")) {
				meta.add(":hxGen", [], baseType.pos);
			}
			baseType.exclude();
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
					excludeBaseType(b);
			}
		});
	}

	/**
		Exclude classes listed in an extern file (one per line) from being generated.
	**/
	public static function excludeFile( fileName : String ) {
		fileName = Context.resolvePath(fileName);
		var f = sys.io.File.read(fileName,true);
		var classes = new haxe.ds.StringMap();
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
				case TInst(c, _): if( classes.exists(c.toString()) ) excludeBaseType(c.get());
				case TEnum(e, _): if( classes.exists(e.toString()) ) excludeBaseType(e.get());
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
		var f = sys.io.File.read(file, true);
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
				if( StringTools.startsWith(r, "enum ") ) {
					define("fakeEnum:" + r.substr(5));
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

	/**
		Marks types or packages to be kept by DCE.

		This also extends to the sub-types of resolved modules.

		In order to include module sub-types directly, their full dot path
		including the containing module has to be used
		(e.g. msignal.Signal.Signal0).

		This operation has no effect if the type has already been loaded, e.g.
		through `Context.getType`.

		@param path A package, module or sub-type dot path to keep.
		@param paths An Array of package, module or sub-type dot paths to keep.
		@param recursive If true, recurses into sub-packages for package paths.
	**/
	public static function keep(?path : String, ?paths : Array<String>, ?recursive:Bool = true) {
		if (null == paths)
			paths = [];
		if (null != path)
			paths.push(path);
		for (path in paths) {
			addGlobalMetadata(path, "@:keep", recursive, true, true);
		}
	}

	/**
		Adds metadata `meta` to all types (if `toTypes = true`) or fields (if
		`toFields = true`) whose dot-path matches `pathFilter`.

		If `recursive` is true a dot-path is considered matched if it starts
		with `pathFilter`. This automatically applies to path filters of
		packages. Otherwise an exact match is required.

		If `pathFilter` is the empty String `""` it matches everything (if
		`recursive = true`) or only top-level types (if `recursive = false`).

		This operation has no effect if the type has already been loaded, e.g.
		through `Context.getType`.
	**/
	public static function addGlobalMetadata(pathFilter:String, meta:String, ?recursive:Bool = true, ?toTypes:Bool = true, ?toFields:Bool = false) {
		untyped load("add_global_metadata",5)(untyped pathFilter.__s, meta.__s, recursive, toTypes, toFields);
	}

	/**
		Change the default JS output by using a custom generator callback
	**/
	public static function setCustomJSGenerator( callb : JSGenApi -> Void ) {
		load("custom_js",1)(callb);
	}

	static function load( f, nargs ) : Dynamic {
		#if macro
		return neko.Lib.load("macro", f, nargs);
		#else
		return Reflect.makeVarArgs(function(_) return throw "Can't be called outside of macro");
		#end
	}

#end

	#if (js || macro)
	/**
		Embed an on-disk javascript file (can be called into an __init__ method)
	**/
	public static macro function includeFile( fileName : Expr ) {
		var str = switch( fileName.expr ) {
		case EConst(c):
			switch( c ) {
			case CString(str): str;
			default: null;
			}
		default: null;
		}
		if( str == null ) Context.error("Should be a constant string", fileName.pos);
		var f = try sys.io.File.getContent(Context.resolvePath(str)) catch( e : Dynamic ) Context.error(Std.string(e), fileName.pos);
		var p = Context.currentPos();
		return { expr : EUntyped( { expr : ECall( { expr : EConst(CIdent("__js__")), pos : p }, [ { expr : EConst(CString(f)), pos : p } ]), pos : p } ), pos : p };
	}
	#end

}
