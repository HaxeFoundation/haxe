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

// This file is generated from mozilla\CSSKeyframesRule.webidl. Do not edit!

package js.html;

/**
	The `CSSKeyframesRule` interface describes an object representing a complete set of keyframes for a CSS animation. It corresponds to the contains of a whole `@keyframes` at-rule. It implements the `CSSRule` interface with a type value of `7` (`CSSRule.KEYFRAMES_RULE`).

	Documentation [CSSKeyframesRule](https://developer.mozilla.org/en-US/docs/Web/API/CSSKeyframesRule) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CSSKeyframesRule$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CSSKeyframesRule>
**/
@:native("CSSKeyframesRule")
extern class CSSKeyframesRule extends CSSRule {
	
	/**
		Represents the name of the animation, used by the `animation-name` property.
	**/
	var name : String;
	
	/**
		Returns a `CSSRuleList` of the CSS rules in the media rule.
	**/
	var cssRules(default,null) : CSSRuleList;
	
	
	/**
		Inserts a new keyframe rule into the current CSSKeyframesRule. The parameter is a `DOMString` containing a keyframe in the same format as an entry of a `@keyframes` at-rule. If it contains more than one keyframe rule, a `DOMException` with a `SYNTAX_ERR` is thrown.
	**/
	function appendRule( rule : String ) : Void;
	
	/**
		Deletes a keyframe rule from the current CSSKeyframesRule. The parameter is the index of the keyframe to be deleted, expressed as a `DOMString` resolving as a number between `0%` and `100%`.
	**/
	function deleteRule( select : String ) : Void;
	
	/**
		Returns a keyframe rule corresponding to the given key. The key is a `DOMString` containing an index of the keyframe to be returned, resolving to a percentage between `0%` and `100%`. If no such keyframe exists, `findRule` returns `null`.
	**/
	function findRule( select : String ) : CSSKeyframeRule;
}