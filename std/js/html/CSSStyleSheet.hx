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

// This file is generated from mozilla\CSSStyleSheet.webidl. Do not edit!

package js.html;

/**
	The `CSSStyleSheet` interface represents a single CSS style sheet. It inherits properties and methods from its parent, `StyleSheet`.

	Documentation [CSSStyleSheet](https://developer.mozilla.org/en-US/docs/Web/API/CSSStyleSheet) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CSSStyleSheet$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CSSStyleSheet>
**/
@:native("CSSStyleSheet")
extern class CSSStyleSheet extends StyleSheet {
	
	/**
		If this style sheet is imported into the document using an `@import` rule, the `ownerRule` property will return that `CSSImportRule`, otherwise it returns `null`.
	**/
	var ownerRule(default,null) : CSSRule;
	
	/**
		Returns a live `CSSRuleList`, listing the `CSSRule` objects in the style sheet.
		
		 This is normally used to access individual rules like this:
		
		 `   styleSheet.cssRules[i] // where i = 0..cssRules.length-1`
		
		 To add or remove items in `cssRules`, use the `CSSStyleSheet`'s `deleteRule()` and `insertRule()` methods, described below.
	**/
	var cssRules(default,null) : CSSRuleList;
	
	
	/**
		Inserts a new rule at the specified position in the style sheet, given the textual representation of the rule.
		@throws DOMError
	**/
	function insertRule( rule : String, index : Int = 0 ) : Int;
	
	/**
		Deletes a rule at the specified position from the style sheet.
		@throws DOMError
	**/
	function deleteRule( index : Int ) : Void;
}