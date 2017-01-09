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
package haxe.macro;
import haxe.macro.Expr;

/**
	All these methods can be called for compiler configuration macros.
**/
#if hl
@:hlNative("macro")
#end
class Compiler {
	/**
		A conditional compiler flag can be set command line using
		`-D key=value`.

		Returns the value of a compiler flag.

		If the compiler flag is defined but no value is set,
		`Compiler.getDefine` returns `"1"` (e.g. `-D key`).

		If the compiler flag is not defined, `Compiler.getDefine` returns
		`null`.

		@see https://haxe.org/manual/lf-condition-compilation.html
	**/
	macro static public function getDefine( key : String ) {
		return macro $v{haxe.macro.Context.definedValue(key)};
	}

#if (neko || (macro && hl))

	static var ident = ~/^[A-Za-z_][A-Za-z0-9_]*$/;
	static var path = ~/^[A-Za-z_][A-Za-z0-9_.]*$/;

	public static function allowPackage( v : String ) {
		#if neko
		load("allow_package", 1)(v);
		#end
	}

	/**
		Set a conditional compiler flag.
	**/
	public static function define( flag : String, ?value : String ) {
		#if neko
		load("define", 2)(flag,value);
		#end
	}

	#if !neko
	private static function typePatch( cl : String, f : String, stat : Bool, t : String ) {
	}
	private static function metaPatch( meta : String, cl : String, f : String, stat : Bool ) {
	}
	private static function addGlobalMetadataImpl(pathFilter:String, meta:String, recursive:Bool, toTypes:Bool, toFields:Bool) {
	}
	#end

	/**
		Removes a (static) field from a given class by name.
		An error is thrown when `className` or `field` is invalid.
	**/
	public static function removeField( className : String, field : String, ?isStatic : Bool ) {
		if( !path.match(className) ) throw "Invalid "+className;
		if( !ident.match(field) ) throw "Invalid " + field;
		#if neko
		load("type_patch", 4)(className, field, isStatic == true, null);
		#else
		typePatch(className, field, isStatic == true, null);
		#end
	}

	/**
		Set the type of a (static) field at a given class by name.
		An error is thrown when `className` or `field` is invalid.
	**/
	public static function setFieldType( className : String, field : String, type : String, ?isStatic : Bool ) {
		if( !path.match(className) ) throw "Invalid "+className;
		if( !ident.match((field.charAt(0) == "$") ? field.substr(1) : field) ) throw "Invalid "+field;
		#if neko
		load("type_patch", 4)(className, field, isStatic == true, type);
		#else
		typePatch(className, field, isStatic == true, type);
		#end
	}

	/**
		Add metadata to a (static) field or class by name.
		An error is thrown when `className` or `field` is invalid.
	**/
	public static function addMetadata( meta : String, className : String, ?field : String, ?isStatic : Bool ) {
		if( !path.match(className) ) throw "Invalid "+className;
		if( field != null && !ident.match(field) ) throw "Invalid "+field;
		#if neko
		load("meta_patch", 4)(meta, className, field, isStatic == true);
		#else
		metaPatch(meta, className, field, isStatic == true);
		#end
	}

	public static function addClassPath( path : String ) {
		#if neko
		load("add_class_path", 1)(path);
		#end
	}

	public static function getOutput() : String {
		#if neko
		return load("get_output", 0)();
		#else
		return null;
		#end
	}

	public static function setOutput( fileOrDir : String ) {
		#if neko
		load("set_output", 1)(fileOrDir);
		#end
	}

	public static function getDisplayPos() : Null<{ file : String, pos : Int }> {
		#if neko
		return load("get_display_pos", 0)();
		#else
		return null;
		#end
	}

	/**
		Adds a native library depending on the platform (e.g. `-swf-lib` for Flash).
	**/
	public static function addNativeLib( name : String ) {
		#if neko
		load("add_native_lib", 1)(name);
		#end
	}

	/**
		Adds an argument to be passed to the native compiler (e.g. `-javac-arg` for Java).
	 **/
	public static function addNativeArg( argument : String ) {
		#if neko
		load("add_native_arg", 1)(argument);
		#end
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
		@param strict If true and given package wasn't found in any of class paths, fail with an error.
	**/
	public static function include( pack : String, ?rec = true, ?ignore : Array<String>, ?classPaths : Array<String>, strict = false ) {
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
		var found = false;
		for( cp in classPaths ) {
			var path = pack == '' ? cp : cp + "/" + pack.split(".").join("/");
			if( !sys.FileSystem.exists(path) || !sys.FileSystem.isDirectory(path) )
				continue;
			found = true;
			for( file in sys.FileSystem.readDirectory(path) ) {
				if( StringTools.endsWith(file, ".hx") && file.substr(0, file.length - 3).indexOf(".") < 0 ) {
					var cl = prefix + file.substr(0, file.length - 3);
					if( skip(cl) )
						continue;
					Context.getModule(cl);
				} else if( rec && sys.FileSystem.isDirectory(path + "/" + file) && !skip(prefix + file) )
					include(prefix + file, true, ignore, classPaths);
			}
		}
		if (strict && !found)
			Context.error('Package "$pack" was not found in any of class paths', Context.currentPos());
	}

	/**
		Exclude a class or an enum without changing it to `@:nativeGen`.
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
		Exclude a specific class, enum, or all classes and enums in a
		package from being generated. Excluded types become `extern`.

		@param rec If true, recursively excludes all sub-packages.
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
		Exclude classes and enums listed in an extern file (one per line) from being generated.
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
		Load a type patch file that can modify the field types within declared classes and enums.
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
		(e.g. `msignal.Signal.Signal0`).

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
		#if neko
		load("add_global_metadata_impl", 5)(pathFilter, meta, recursive, toTypes, toFields);
		#else
		addGlobalMetadataImpl(pathFilter, meta, recursive, toTypes, toFields);
		#end
	}

	/**
		Change the default JS output by using a custom generator callback
	**/
	public static function setCustomJSGenerator( callb : JSGenApi -> Void ) {
		#if neko
		load("set_custom_js_generator", 1)(callb);
		#end
	}

	#if neko
	static inline function load( f, nargs ) : Dynamic {
		return @:privateAccess Context.load(f, nargs);
	}
	#end

#end

	#if (js || lua || macro)
	/**
		Embed a JavaScript file at compile time (can be called by `--macro` or within an `__init__` method).
	**/
	public static #if !macro macro #end function includeFile( file : String, position:IncludePosition = Top ) {
		return switch ((position:String).toLowerCase()) {
			case Inline:
				if (Context.getLocalModule() == "")
					Context.error("Cannot use inline mode when includeFile is called by `--macro`", Context.currentPos());

				var f = try sys.io.File.getContent(Context.resolvePath(file)) catch( e : Dynamic ) Context.error(Std.string(e), Context.currentPos());
				var p = Context.currentPos();
				{ expr : EUntyped( { expr : ECall( { expr : EConst(CIdent("__js__")), pos : p }, [ { expr : EConst(CString(f)), pos : p } ]), pos : p } ), pos : p };
			case Top | Closure:
				@:privateAccess Context.includeFile(file, position);
				macro {};
			case _:
				Context.error("unknown includeFile position: " + position, Context.currentPos());
		}
	}

	#end

}

@:enum abstract IncludePosition(String) from String to String {
	/**
		Prepend the file content to the output file.
	*/
	var Top = "top";
	/**
		Prepend the file content to the body of the top-level closure.

		Since the closure is in strict-mode, there may be run-time error if the input is not strict-mode-compatible.
	*/
	var Closure = "closure";
	/**
		Directly inject the file content at the call site.
	*/
	var Inline = "inline";
}
