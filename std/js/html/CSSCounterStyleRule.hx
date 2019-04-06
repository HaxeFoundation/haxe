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

// This file is generated from mozilla\CSSCounterStyleRule.webidl. Do not edit!

package js.html;

/**
	The `CSSCounterStyleRule` interface represents an `@counter-style` at-rule.

	Documentation [CSSCounterStyleRule](https://developer.mozilla.org/en-US/docs/Web/API/CSSCounterStyleRule) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CSSCounterStyleRule$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CSSCounterStyleRule>
**/
@:native("CSSCounterStyleRule")
extern class CSSCounterStyleRule extends CSSRule {
	
	/**
		Is a `DOMString` object that contains the serialization of the `counter-style-name` defined for the associated rule.
	**/
	var name : String;
	
	/**
		Is a `DOMString` object that contains the serialization of the `@counter-style/system` descriptor defined for the associated rule. If the descriptor was not specified in the associated rule, the attribute returns an empty string.
	**/
	var system : String;
	
	/**
		Is a `DOMString` object that contains the serialization of the `@counter-style/symbols` descriptor defined for the associated rule. If the descriptor was not specified in the associated rule, the attribute returns an empty string.
	**/
	var symbols : String;
	
	/**
		Is a `DOMString` object that contains the serialization of the `@counter-style/additive-symbols` descriptor defined for the associated rule. If the descriptor was not specified in the associated rule, the attribute returns an empty string.
	**/
	var additiveSymbols : String;
	
	/**
		Is a `DOMString` object that contains the serialization of the `@counter-style/negative` descriptor defined for the associated rule. If the descriptor was not specified in the associated rule, the attribute returns an empty string.
	**/
	var negative : String;
	
	/**
		Is a `DOMString` object that contains the serialization of the `@counter-style/prefix` descriptor defined for the associated rule. If the descriptor was not specified in the associated rule, the attribute returns an empty string.
	**/
	var prefix : String;
	
	/**
		Is a `DOMString` object that contains the serialization of the `@counter-style/suffix` descriptor defined for the associated rule. If the descriptor was not specified in the associated rule, the attribute returns an empty string.
	**/
	var suffix : String;
	
	/**
		Is a `DOMString` object that contains the serialization of the `@counter-style/range` descriptor defined for the associated rule. If the descriptor was not specified in the associated rule, the attribute returns an empty string.
	**/
	var range : String;
	
	/**
		Is a `DOMString` object that contains the serialization of the `@counter-style/pad` descriptor defined for the associated rule. If the descriptor was not specified in the associated rule, the attribute returns an empty string.
	**/
	var pad : String;
	
	/**
		Is a `DOMString` object that contains the serialization of the `@counter-style/speak-as` descriptor defined for the associated rule. If the descriptor was not specified in the associated rule, the attribute returns an empty string.
	**/
	var speakAs : String;
	
	/**
		Is a `DOMString` object that contains the serialization of the `@counter-style/fallback` descriptor defined for the associated rule. If the descriptor was not specified in the associated rule, the attribute returns an empty string.
	**/
	var fallback : String;
	
}