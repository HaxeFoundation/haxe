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

import haxe.DynamicAccess;

@:native("require")
extern class Require {
	/**
		Used to import modules, `JSON`, and local files.
		Modules can be imported from `node_modules`.
		Local modules and JSON files can be imported using a relative path (e.g. `./`, .`/foo`, `./bar/baz`, `../foo`)
		that will be resolved against the directory named by `__dirname` (if defined) or the current working directory.

		@see https://nodejs.org/api/modules.html#modules_module_id
	**/
	@:selfCall
	static function require(id:String):Dynamic;

	/**
		Modules are cached in this object when they are required.
		By deleting a key value from this object, the next `require` will reload the module.
		This does not apply to native addons, for which reloading will result in an error.

		@see https://nodejs.org/api/modules.html#modules_require_cache
	**/
	static var cache(default, null):DynamicAccess<Module>;

	/**
		Instruct require on how to handle certain file extensions.

		Deprecated: In the past, this list has been used to load non-JavaScript modules into Node by compiling them on-demand.
		However, in practice, there are much better ways to do this, such as loading modules via some other Node program,
		or compiling them to JavaScript ahead of time.

		Since the `Module` system is locked, this feature will probably never go away. However, it may have subtle bugs
		and complexities that are best left untouched.
	**/
	@:deprecated
	static var extensions(default, null):DynamicAccess<Dynamic>;

	/**
		The `Module` object representing the entry script loaded when the Node.js process launched.
		See ["Accessing the main module"](https://nodejs.org/api/modules.html#modules_accessing_the_main_module).

		@see https://nodejs.org/api/modules.html#modules_require_main
	**/
	static var main(default, null):Module;

	/**
		Use the internal `require()` machinery to look up the location of a module,
		but rather than loading the module, just return the resolved filename.

		@see https://nodejs.org/api/modules.html#modules_require_resolve_request_options
	**/
	static function resolve(module:String, ?options:RequireResolveOptions):String;
}

@:native("require.resolve")
extern class RequireResolve {
	/**
		Use the internal `require()` machinery to look up the location of a module,
		but rather than loading the module, just return the resolved filename.

		@see https://nodejs.org/api/modules.html#modules_require_resolve_request_options
	**/
	@:selfCall
	static function resolve(module:String, ?options:RequireResolveOptions):String;

	/**
		Returns an array containing the paths searched during resolution of `request` or `null`
		if the `request` string references a core module, for example `http` or `fs`.

		@see https://nodejs.org/api/modules.html#modules_require_resolve_paths_request
	**/
	static function paths(request:String):Null<Array<String>>;
}

typedef RequireResolveOptions = {
	/**
		Paths to resolve module location from.
		If present, these paths are used instead of the default resolution paths,
		with the exception of `GLOBAL_FOLDERS` like $HOME/.node_modules, which are always included.
		Each of these paths is used as a starting point for the module resolution algorithm,
		meaning that the node_modules hierarchy is checked from this location.
	**/
	@:optional var paths:Array<String>;
}
