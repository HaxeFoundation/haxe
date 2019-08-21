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

// This file is generated from mozilla\CSSNamespaceRule.webidl. Do not edit!

package js.html;

/**
	The `CSSNamespaceRule` interface describes an object representing a single CSS `@namespace` at-rule. It implements the `CSSRule` interface, with a type value of `10` (`CSSRule.NAMESPACE_RULE`).

	Documentation [CSSNamespaceRule](https://developer.mozilla.org/en-US/docs/Web/API/CSSNamespaceRule) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CSSNamespaceRule$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CSSNamespaceRule>
**/
@:native("CSSNamespaceRule")
extern class CSSNamespaceRule extends CSSRule {
	
	/**
		Returns a `DOMString` containing the text of the URI of the given namespace.
	**/
	var namespaceURI(default,null) : String;
	
	/**
		Returns a `DOMString` with the name of the prefix associated to this namespace. If there is no such prefix, returns  `null`.
	**/
	var prefix(default,null) : String;
	
}