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

import haxe.macro.Compiler;

enum abstract ModuleCheckPolicy(Int) {
	/**
		Disables file modification checks, avoiding some filesystem operations.
	**/
	var NoCheckFileTimeModification = 0;

	/**
		If a file is modified, also checks if its content changed. This check
		is not free, but useful when .hx files are auto-generated.
	**/
	var CheckFileContentModification = 1;

	/**
		Disables dependency checks of the module.

		This should only be used for modules that don't depend on any module that
		might change. It is effectively a promise to the compiler that the module
		is unaffected by changes made to other modules. If that promise is broken,
		the compiler is sad and things probably stop working.
	**/
	var NoCheckDependencies = 2;

	/**
		Disables file shadowing checks. Shadowing can occur when a new file
		is added to a class-path that has higher priority than the class-path
		of the current module file.
	**/
	var NoCheckShadowing = 3;
	/**
		Retype the module's contents if its file is invalidated. This is currently experimental.
	**/
	var Retype = 4;
}

enum abstract ContextOptions(Int) {
	/**
		Affects only the normal context.
	**/
	var NormalContext = 0;

	/**
		Affects only the macro context.
	**/
	var MacroContext = 1;

	/**
		Affects the normal and macro contexts.
	**/
	var NormalAndMacroContext = 2;
}

/**
	This class provides some methods which can be invoked from command line using
	`--macro server.field(args)`.
**/
class CompilationServer {
	#if (macro || display)
	/**
		Sets the `ModuleCheckPolicy` of all files whose dot-path matches an
		element of `pathFilters`.

		If `recursive` is true, a dot-path is considered matched if it starts
		with the path filter. This automatically applies to path filters of
		packages. Otherwise an exact match is required.

		If an element in `pathFilters` is the empty String `""` it matches
		everything (if `recursive = true`) or only top-level types (if
		`recursive = false`).

		The argument `contextOptions` determines which context (normal, macro
		or both) this affects.

		If a call to this function is added to the compilation parameters, the
		compilation server should be restarted to ensure it takes effect.
	**/
	static public function setModuleCheckPolicy(pathFilters:Array<String>, policy:Array<ModuleCheckPolicy>, ?recursive = true,
			?contextOptions:ContextOptions = NormalContext) {
		@:privateAccess Compiler.load("server_add_module_check_policy", 4)(pathFilters, policy, recursive, contextOptions);
	}

	/**
		Invalidates all files given in `filePaths`, removing them from the cache.
	**/
	static public function invalidateFiles(filePaths:Array<String>) {
		@:privateAccess Compiler.load("server_invalidate_files", 1)(filePaths);
	}
	#end
}
