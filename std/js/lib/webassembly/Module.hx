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

package js.lib.webassembly;

import js.lib.BufferSource;

/**
	A WebAssembly `Module` object contains stateless WebAssembly code that has already
	been compiled by the browser and can be efficiently [shared with Workers](https://developer.mozilla.org/en-US/docs/Web/API/Worker/postMessage),
	and instantiated multiple times. To instantiate the module, call
	[the secondary overload of `WebAssembly.instantiate()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly/instantiate#Secondary_overload_%E2%80%94_taking_a_module_object_instance).

	Documentation [Module](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly/Module) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly/Module$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("WebAssembly.Module")
extern class Module {
	@:pure function new(bufferSource:BufferSource):Void;

	/**
		Given a `Module` and string, returns a copy of the contents of all custom sections
		in the module with the given string name.
	**/
	@:pure static function customSections(module:Module, sectionName:String):Array<ArrayBuffer>;

	/**
		Given a `Module`, returns an array containing descriptions of all the declared exports.
	**/
	@:pure static function exports(module:Module):Array<ModuleExportDescriptor>;

	/**
		Given a `Module`, returns an array containing descriptions of all the declared imports.
	**/
	@:pure static function imports(module:Module):Array<ModuleImportDescriptor>;
}

typedef ModuleExportDescriptor = {
	var name:String;
	var kind:ImportExportKind;
}

typedef ModuleImportDescriptor = {
	var module:String;
	var name:String;
	var kind:ImportExportKind;
}

enum abstract ImportExportKind(String) {
	var Function = "function";
	var Table = "table";
	var Memory = "memory";
	var Global = "global";
}
