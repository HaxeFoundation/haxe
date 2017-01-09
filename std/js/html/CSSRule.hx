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

// This file is generated from cssrule.webidl. Do not edit!

package js.html;

/**
	The `CSSRule` interface represents a single CSS rule. There are several types of rules, listed in the Type constants section below.

	Documentation [CSSRule](https://developer.mozilla.org/en-US/docs/Web/API/CSSRule) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CSSRule$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CSSRule>
**/
@:native("CSSRule")
extern class CSSRule
{
	static inline var UNKNOWN_RULE : Int = 0;
	static inline var STYLE_RULE : Int = 1;
	static inline var CHARSET_RULE : Int = 2;
	static inline var IMPORT_RULE : Int = 3;
	static inline var MEDIA_RULE : Int = 4;
	static inline var FONT_FACE_RULE : Int = 5;
	static inline var PAGE_RULE : Int = 6;
	
	var type(default,null) : Int;
	var cssText : String;
	var parentStyleSheet(default,null) : CSSStyleSheet;
	var parentRule(default,null) : CSSRule;
	
}