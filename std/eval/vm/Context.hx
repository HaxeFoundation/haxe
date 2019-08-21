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

package eval.vm;

extern class Context {
	static function addBreakpoint(file:String, line:Int):Void;
	static function breakHere():Void;
	static function callMacroApi(s:String):Dynamic;

	/**
		Loads and returns a plugin from file `filePath`.

		If Haxe is built natively, the extension automatically defaults to `.cmxs`,
		even if a different extension is provided in `filePath`. In bytecode mode,
		the default extension is `.cmo`.

		Sample plugin:

		```ocaml
		open EvalValue
		open EvalContext
		open EvalEncode

		let add_int = vfun2 (fun v1 v2 -> match v1,v2 with
			| VInt32 i1,VInt32 i2 -> vint32 (Int32.add i1 i2)
			| _ -> exc_string "Expected int + int"
		)
		;;
		EvalStdLib.StdContext.register ["add_int",add_int]
		```

		Usage from Haxe:

		```haxe
		var module:TestPlugin = eval.vm.Context.loadPlugin("testPlugin.cmo");
		trace(module.add_int(4, 3));
		```

		Plugins have to be compiled with the same OCaml version as the Haxe compiler
		and using the same Haxe version. If a plugin cannot be loaded, an exception
		of type `String` is thrown.
	**/
	static function loadPlugin<T>(filePath:String):T;
}
