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

/**
	This class can be added via `using haxe.macro.Tools` in order to enable
	`using` functionality on these macro tool classes:

	- `haxe.macro.ExprTools`
	- `haxe.macro.ComplexTypeTools`
	- `haxe.macro.TypeTools`
	- `haxe.macro.MacroStringTools`
	- `haxe.macro.TypedExprTools`
	- `haxe.macro.PositionTools`
  
  @see <https://haxe.org/manual/lf-static-extension.html>
**/
@:dox(hide)
typedef TExprTools = ExprTools;

@:dox(hide)
typedef TComplexTypeTools = ComplexTypeTools;

@:dox(hide)
typedef TTypeTools = TypeTools;

@:dox(hide)
typedef TMacroStringTools = MacroStringTools;

@:dox(hide)
typedef TTypedExprTools = TypedExprTools;

@:dox(hide)
typedef TPositionTools = PositionTools;
