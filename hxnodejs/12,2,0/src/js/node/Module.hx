/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node;

import js.node.url.URL;

/**
	In each module, the `module` free variable is a reference to the object representing the current module.
	For convenience, `module.exports` is also accessible via the `exports` module-global.
	`module` is not actually a global but rather local to each module.

	@see https://nodejs.org/api/modules.html#modules_the_module_object
**/
@:jsRequire("module")
extern class Module {
	/**
		The module objects required for the first time by this one.

		@see https://nodejs.org/api/modules.html#modules_module_children
	**/
	var children(default, null):Array<Module>;

	/**
		The `module.exports` object is created by the Module system.
		Sometimes this is not acceptable; many want their module to be an instance of some class.
		To do this, assign the desired export object to `module.exports`.
		Assigning the desired object to `exports` will simply rebind the local `exports` variable, which is probably not
		what is desired.

		@see https://nodejs.org/api/modules.html#modules_module_exports
	**/
	var exports:Dynamic;

	/**
		The fully resolved filename of the module.

		@see https://nodejs.org/api/modules.html#modules_module_filename
	**/
	var filename(default, null):String;

	/**
		The identifier for the module.
		Typically this is the fully resolved filename.

		@see https://nodejs.org/api/modules.html#modules_module_id
	**/
	var id(default, null):String;

	/**
		Whether or not the module is done loading, or is in the process of loading.

		@see https://nodejs.org/api/modules.html#modules_module_loaded
	**/
	var loaded(default, null):Bool;

	/**
		The module that first required this one.

		@see https://nodejs.org/api/modules.html#modules_module_parent
	**/
	var parent(default, null):Module;

	/**
		The search paths for the module.

		@see https://nodejs.org/api/modules.html#modules_module_paths
	**/
	var paths(default, null):Array<String>;

	/**
		The `module.require()` method provides a way to load a module as if `require()` was called from the original
		module.

		@see https://nodejs.org/api/modules.html#modules_module_require_id
	**/
	function require(id:String):Dynamic;

	/**
		A list of the names of all modules provided by Node.js.
		Can be used to verify if a module is maintained by a third party or not.

		@see https://nodejs.org/api/modules.html#modules_module_builtinmodules
	**/
	static var builtinModules(default, null):Array<String>;

	/**
		@see https://nodejs.org/api/modules.html#modules_module_createrequire_filename
	**/
	@:overload(function(filename:URL):String->Dynamic {})
	static function createRequire(filename:String):String->Dynamic;

	/**
		The `module.syncBuiltinESMExports()` method updates all the live bindings for builtin ES Modules to match the
		properties of the CommonJS exports.
		It does not add or remove exported names from the ES Modules.

		@see https://nodejs.org/api/modules.html#modules_module_syncbuiltinesmexports
	**/
	static function syncBuiltinESMExports():Void;
}
