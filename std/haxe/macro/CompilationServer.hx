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
	var NoFileSystemCheck = 0;

	/**
		Default behavior: check last modification time.
	**/
	var CheckFileModificationTime = 1;

	/**
		If a file is modified, also checks if its content changed. This check
		is not free, but useful when .hx files are auto-generated.
	**/
	var CheckFileContentModification = 2;
}

/**
	This class provides some methods which can be invoked from command line using
	`--macro server.field(args)`.
**/
class CompilationServer {
	#if macro
	/**
		Sets the `ModuleCheckPolicy` of all files whose dot-path matches an
		element of `pathFilters`.

		If `recursive` is true, a dot-path is considered matched if it starts
		with the path filter. This automatically applies to path filters of
		packages. Otherwise an exact match is required.

		If an element in `pathFilters` is the empty String `""` it matches
		everything (if `recursive = true`) or only top-level types (if
		`recursive = false`).

		If a call to this function is added to the compilation parameters, the
		compilation server should be restarted to ensure it takes effect.
	**/
	static public function setModuleCheckPolicy(pathFilters:Array<String>, policy:Array<ModuleCheckPolicy>, ?recursive = true) {
		Context.onAfterInitMacros(() -> {
			@:privateAccess Compiler.load("server_add_module_check_policy", 4)(pathFilters, policy, recursive);
		});
	}

	/**
		Invalidates all files given in `filePaths`, removing them from the cache.
	**/
	static public function invalidateFiles(filePaths:Array<String>) {
		@:privateAccess Compiler.load("server_invalidate_files", 1)(filePaths);
	}
	#end
}
