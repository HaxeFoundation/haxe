/*
 * Copyright (C)2005-2019 Haxe Foundation
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

import haxe.display.Display;
import haxe.macro.Expr;
import haxe.hxb.WriterConfig;

/**
	All these methods can be called for compiler configuration macros.
**/
class Compiler {
	/**
		A conditional compilation flag can be set on the command line using
		`-D key=value`.

		Returns the value of a compiler flag.

		If the compiler flag is defined but no value is set,
		`Compiler.getDefine` returns `"1"` (e.g. `-D key`).

		If the compiler flag is not defined, `Compiler.getDefine` returns
		`null`.

		Note: This is a macro and cannot be called from within other macros. Refer
		to `haxe.macro.Context.definedValue` to obtain defined values in macro context.

		@see https://haxe.org/manual/lf-condition-compilation.html
	**/
	macro /* <-- ! */ static public function getDefine(key:String) {
		return macro $v{haxe.macro.Context.definedValue(key)};
	}

	#if (neko || (macro && hl) || (macro && eval))
	static var ident = ~/^[A-Za-z_][A-Za-z0-9_]*$/;
	static var path = ~/^[A-Za-z_][A-Za-z0-9_.]*$/;

	public static function allowPackage(v:String) {
		#if (neko || eval)
		load("allow_package", 1)(v);
		#end
	}

	/**
		Set a conditional compiler flag.

		Usage of this function outside of initialization macros is deprecated and may cause compilation server issues.
	**/
	public static function define(flag:String, ?value:String) {
		#if (neko || eval)
		Context.assertInitMacro();
		load("define", 2)(flag, value);
		#end
	}

	#if (!neko && !eval)
	private static function addGlobalMetadataImpl(pathFilter:String, meta:String, recursive:Bool, toTypes:Bool, toFields:Bool) {}
	#end

	/**
		Add a class path where ".hx" source files or packages (sub-directories) can be found.

		Usage of this function outside of initialization macros is deprecated and may cause compilation server issues.
	**/
	public static function addClassPath(path:String) {
		#if (neko || eval)
		Context.assertInitMacro();
		load("add_class_path", 1)(path);
		#end
	}

	public static function getOutput():String {
		#if (neko || eval)
		return load("get_output", 0)();
		#else
		return null;
		#end
	}

	public static function setOutput(fileOrDir:String) {
		#if (neko || eval)
		load("set_output", 1)(fileOrDir);
		#end
	}

	public static function getDisplayPos():Null<{file:String, pos:Int}> {
		#if (neko || eval)
		return load("get_display_pos", 0)();
		#else
		return null;
		#end
	}

	/**
		Returns all the configuration settings applied to the compiler.

		Usage of this function outside a macro context returns `null`.
	**/
	public static function getConfiguration():Null<CompilerConfiguration> {
		#if (neko || eval)
		return load("get_configuration", 0)();
		#else
		return null;
		#end
	}

	/**
		Sets the target configuration.

		Usage of this function outside a macro context does nothing.
	**/
	public static function setPlatformConfiguration(config:PlatformConfig):Void {
		#if (neko || eval)
		load("set_platform_configuration", 1)(config);
		#end
	}

	/**
		Adds a native library depending on the platform (e.g. `-swf-lib` for Flash).

		Usage of this function outside of initialization macros is deprecated and may cause compilation server issues.
	**/
	public static function addNativeLib(name:String) {
		#if (neko || eval)
		Context.assertInitMacro();
		load("add_native_lib", 1)(name);
		#end
	}

	/**
		Includes all modules in package `pack` in the compilation.

		In order to include single modules, their paths can be listed directly
		on command line: `haxe ... ModuleName pack.ModuleName`.

		By default `Compiler.include` will search for modules in the directories defined with `-cp`.
		If you want to specify a different set of paths to search for modules, you can use the optional
		argument `classPath`.

		@param pack The package dot-path as String. Use `''` to include the root package.
		@param rec If true, recursively adds all sub-packages.
		@param ignore Array of module names to ignore for inclusion.
			   You can use `module*` with a * at the end for Wildcard matching
		@param classPaths An alternative array of paths (directory names) to use to search for modules to include.
			   Note that if you pass this argument, only the specified paths will be used for inclusion.
		@param strict If true and given package wasn't found in any of class paths, fail with an error.
	**/
	public static function include(pack:String, ?rec = true, ?ignore:Array<String>, ?classPaths:Array<String>, strict = false) {
		function include(pack:String, ?rec = true, ?ignore:Array<String>, ?classPaths:Array<String>, strict = false) {
			var ignoreWildcard:Array<String> = [];
			var ignoreString:Array<String> = [];
			if (ignore != null) {
				for (ignoreRule in ignore) {
					if (StringTools.endsWith(ignoreRule, "*")) {
						ignoreWildcard.push(ignoreRule.substr(0, ignoreRule.length - 1));
					} else {
						ignoreString.push(ignoreRule);
					}
				}
			}
			var skip = if (ignore == null) {
				function(c) return false;
			} else {
				function(c:String) {
					if (Lambda.has(ignoreString, c))
						return true;
					for (ignoreRule in ignoreWildcard)
						if (StringTools.startsWith(c, ignoreRule))
							return true;
					return false;
				}
			}
			var displayValue = Context.definedValue("display");
			if (classPaths == null) {
				classPaths = Context.getClassPath();
				// do not force inclusion when using completion
				switch (displayValue) {
					case null:
					case "usage":
					case _:
						return;
				}
				// normalize class path
				for (i in 0...classPaths.length) {
					var cp = StringTools.replace(classPaths[i], "\\", "/");
					if (StringTools.endsWith(cp, "/"))
						cp = cp.substr(0, -1);
					if (cp == "")
						cp = ".";
					classPaths[i] = cp;
				}
			}
			var prefix = pack == '' ? '' : pack + '.';
			var found = false;
			for (cp in classPaths) {
				var path = pack == '' ? cp : cp + "/" + pack.split(".").join("/");
				if (!sys.FileSystem.exists(path) || !sys.FileSystem.isDirectory(path))
					continue;
				found = true;
				for (file in sys.FileSystem.readDirectory(path)) {
					if (StringTools.endsWith(file, ".hx") && file.substr(0, file.length - 3).indexOf(".") < 0) {
						if (file == "import.hx")
							continue;
						var cl = prefix + file.substr(0, file.length - 3);
						if (skip(cl))
							continue;
						Context.getModule(cl);
					} else if (rec && sys.FileSystem.isDirectory(path + "/" + file) && !skip(prefix + file))
						include(prefix + file, true, ignore, classPaths);
				}
			}
			if (strict && !found)
				Context.error('Package "$pack" was not found in any of class paths', Context.currentPos());
		}

		Context.onAfterInitMacros(() -> include(pack, rec, ignore, classPaths, strict));
	}

	/**
		Exclude a class or an enum without changing it to `@:nativeGen`.
	**/
	static function excludeBaseType(baseType:Type.BaseType):Void {
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

		@param pack The package dot-path as String. Use `''` to exclude the root package.
		@param rec If true, recursively excludes all sub-packages.
	**/
	public static function exclude(pack:String, ?rec = true) {
		Context.onGenerate(function(types) {
			for (t in types) {
				var b:Type.BaseType, name;
				switch (t) {
					case TInst(c, _):
						name = c.toString();
						b = c.get();
					case TEnum(e, _):
						name = e.toString();
						b = e.get();
					default:
						continue;
				}
				var p = b.pack.join(".");
				if ((p == pack || name == pack) || (rec && StringTools.startsWith(p, pack + ".")))
					excludeBaseType(b);
			}
		}, false);
	}

	/**
		Exclude classes and enums listed in an extern file (one per line) from being generated.
	**/
	public static function excludeFile(fileName:String) {
		fileName = Context.resolvePath(fileName);
		var f = sys.io.File.read(fileName, true);
		var classes = new haxe.ds.StringMap();
		try {
			while (true) {
				var l = StringTools.trim(f.readLine());
				if (l == "" || !~/[A-Za-z0-9._]/.match(l))
					continue;
				classes.set(l, true);
			}
		} catch (e:haxe.io.Eof) {}
		Context.onGenerate(function(types) {
			for (t in types) {
				switch (t) {
					case TInst(c, _):
						if (classes.exists(c.toString()))
							excludeBaseType(c.get());
					case TEnum(e, _):
						if (classes.exists(e.toString()))
							excludeBaseType(e.get());
					default:
				}
			}
		});
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
	public static function keep(?path:String, ?paths:Array<String>, ?recursive:Bool = true) {
		if (null == paths)
			paths = [];
		if (null != path)
			paths.push(path);
		for (path in paths) {
			addGlobalMetadata(path, "@:keep", recursive, true, true);
		}
	}

	/**
		Enables null safety for a type or a package.

		@param path A package, module or sub-type dot path to enable null safety for.
		@param recursive If true, recurses into sub-packages for package paths.
	**/
	public static function nullSafety(path:String, mode:NullSafetyMode = Loose, recursive:Bool = true) {
		addGlobalMetadata(path, '@:nullSafety($mode)', recursive);
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
		#if (neko || eval)
		load("add_global_metadata_impl", 5)(pathFilter, meta, recursive, toTypes, toFields);
		#else
		addGlobalMetadataImpl(pathFilter, meta, recursive, toTypes, toFields);
		#end
	}

	@:deprecated
	public static function addMetadata(meta:String, className:String, ?field:String, ?isStatic:Bool) {
		var pathFilter = field == null ? className : '$className.$field';
		addGlobalMetadata(pathFilter, meta, false, field == null, field != null);
	}

	/**
		Reference a json file describing user-defined metadata
		See https://github.com/HaxeFoundation/haxe/blob/development/src-json/meta.json
	**/
	public static function registerMetadataDescriptionFile(path:String, ?source:String):Void {
		var f = sys.io.File.getContent(path);
		var content:Array<MetadataDescription> = haxe.Json.parse(f);
		for (m in content)
			registerCustomMetadata(m, source);
	}

	/**
		Reference a json file describing user-defined defines
		See https://github.com/HaxeFoundation/haxe/blob/development/src-json/define.json
	**/
	public static function registerDefinesDescriptionFile(path:String, ?source:String):Void {
		var f = sys.io.File.getContent(path);
		var content:Array<DefineDescription> = haxe.Json.parse(f);
		for (d in content)
			registerCustomDefine(d, source);
	}

	/**
		Register a custom medatada for documentation and completion purposes
	**/
	public static function registerCustomMetadata(meta:MetadataDescription, ?source:String):Void {
		#if (neko || eval)
		load("register_metadata_impl", 2)(meta, source);
		#end
	}

	/**
		Register a custom define for documentation purposes
	**/
	public static function registerCustomDefine(define:DefineDescription, ?source:String):Void {
		#if (neko || eval)
		load("register_define_impl", 2)(define, source);
		#end
	}

	/**
		Change the default JS output by using a custom generator callback
	**/
	public static function setCustomJSGenerator(callb:JSGenApi->Void) {
		#if (neko || eval)
		load("set_custom_js_generator", 1)(callb);
		#end
	}

	#if (neko || eval)
	static inline function load(f, nargs):Dynamic {
		return @:privateAccess Context.load(f, nargs);
	}
	#end

	/**
		Clears cached results of file lookups
	**/
	public static function flushDiskCache() {
		#if (neko || eval)
		load("flush_disk_cache", 0)();
		#end
	}
	#end

	#if (js || lua || macro)
	/**
		Embed a JavaScript or Lua file at compile time (can be called by `--macro` or within an `__init__` method).
	**/
	public static #if !macro macro #end function includeFile(file:String, position:IncludePosition = Top) {
		return switch ((position : String).toLowerCase()) {
			case Inline:
				if (Context.getLocalModule() == "")
					Context.error("Cannot use inline mode when includeFile is called by `--macro`", Context.currentPos());

				var f = try sys.io.File.getContent(Context.resolvePath(file)) catch (e:Dynamic) Context.error(Std.string(e), Context.currentPos());
				var p = Context.currentPos();
				if (Context.defined("js")) {
					macro @:pos(p) js.Syntax.plainCode($v{f});
				} else {
					macro @:pos(p) untyped __lua__($v{f});
				}
			case Top | Closure:
				@:privateAccess Context.includeFile(file, position);
				macro {};
			case _:
				Context.error("unknown includeFile position: " + position, Context.currentPos());
		}
	}
	#end

	/**
		Gets the current hxb writer configuration, if any.
	**/
	static public function getHxbWriterConfiguration():Null<WriterConfig> {
		#if macro
		return load("get_hxb_writer_config", 0)();
		#else
		return null;
		#end
	}

	/**
		Sets the hxb writer configuration to `config`. If no hxb writer configuration
		exists, it is created.

		The intended usage is

		```
		var config = Compiler.getHxbWriterConfiguration();
		config.archivePath = "newPath.zip";
		// Other changes
		Compiler.setHxbWriterConfiguration(config);
		```

		If `config` is `null`, hxb writing is disabled.

		@see haxe.hxb.WriterConfig
	**/
	static public function setHxbWriterConfiguration(config:Null<WriterConfig>) {
		#if macro
		load("set_hxb_writer_config", 1)(config);
		#end
	}
}

enum abstract IncludePosition(String) from String to String {
	/**
		Prepend the file content to the output file.
	**/
	var Top = "top";

	/**
		Prepend the file content to the body of the top-level closure.

		Since the closure is in strict-mode, there may be run-time error if the input is not strict-mode-compatible.
	**/
	var Closure = "closure";

	/**
		Directly inject the file content at the call site.
	**/
	var Inline = "inline";
}

enum abstract NullSafetyMode(String) to String {
	/**
		Disable null safety.
	**/
	var Off;

	/**
		Loose safety.
		If an expression is checked `!= null`, then it's considered safe even if it could be modified after the check.
		E.g.
		```haxe
		function example(o:{field:Null<String>}) {
			if(o.field != null) {
				mutate(o);
				var notNullable:String = o.field; //no error
			}
		}

		function mutate(o:{field:Null<String>}) {
			o.field = null;
		}
		```
	**/
	var Loose;

	/**
		Full scale null safety.
		If a field is checked `!= null` it stays safe until a call is made or any field of any object is reassigned,
		because that could potentially alter an object of the checked field.
		E.g.
		```haxe
		function example(o:{field:Null<String>}, b:{o:{field:Null<String>}}) {
			if(o.field != null) {
				var notNullable:String = o.field; //no error
				someCall();
				var notNullable:String = o.field; // Error!
			}
			if(o.field != null) {
				var notNullable:String = o.field; //no error
				b.o = {field:null};
				var notNullable:String = o.field; // Error!
			}
		}
		```
	**/
	var Strict;

	/**
		Full scale null safety for a multi-threaded environment.
		With this mode checking a field `!= null` does not make it safe, because it could be changed from another thread
		at the same time or immediately after the check.
		The only nullable thing could be safe are local variables.
	**/
	var StrictThreaded;
}

typedef MetadataDescription = {
	final metadata:String;
	final doc:String;

	/**
		External resources for more information about this metadata.
	**/
	@:optional final links:Array<String>;

	/**
		List (small description) of parameters that this metadata accepts.
	**/
	@:optional final params:Array<String>;

	/**
		Haxe target(s) for which this metadata is used.
	**/
	@:optional final platforms:Array<Platform>;

	/**
		Places where this metadata can be applied.
	**/
	@:optional final targets:Array<MetadataTarget>;
}

typedef DefineDescription = {
	final define:String;
	final doc:String;

	/**
		External resources for more information about this define.
	**/
	@:optional final links:Array<String>;

	/**
		List (small description) of parameters that this define accepts.
	**/
	@:optional final params:Array<String>;

	/**
		Haxe target(s) for which this define is used.
	**/
	@:optional final platforms:Array<Platform>;
}

typedef CompilerConfiguration = {
	/**
		The version integer of the current Haxe compiler build.
	**/
	final version:Int;

	/**
		Returns an array of the arguments passed to the compiler from either the `.hxml` file or the command line.
	**/
	final args:Array<String>;

	/**
		If `--debug` mode is enabled, this is `true`.
	**/
	final debug:Bool;

	/**
		If `--verbose` mode is enabled, this is `true`.
	**/
	final verbose:Bool;

	/**
		If `--no-opt` is enabled, this is `false`.
	**/
	final foptimize:Bool;

	/**
		The target platform.
	**/
	final platform:Platform;

	/**
		The compilation configuration for the target platform.
	**/
	final platformConfig:PlatformConfig;

	/**
		A list of paths being used for the standard library.
	**/
	final stdPath:Array<String>;

	/**
		The path of the class passed using the `-main` argument.
	**/
	final mainClass:TypePath;

	/**
		Special access rules for packages depending on the compiler configuration.

		For example, the "java" package is "Forbidden" when the target platform is Python.
	**/
	final packageRules:Map<String, PackageRule>;
}

enum Platform {
	Cross;
	Js;
	Lua;
	Neko;
	Flash;
	Php;
	Cpp;
	Jvm;
	Python;
	Hl;
	Eval;
	CustomTarget(name:String);
}

enum PackageRule {
	Forbidden;
	Directory(path:String);
	Remap(path:String);
}
