/*
 * Copyright (C)2005-2018 Haxe Foundation
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

// This file is generated from mozilla\MutationEvent.webidl. Do not edit!

package js.html;

/**
	Provides event properties that are specific to modifications to the Document Object Model (DOM) hierarchy and nodes.

	Documentation [MutationEvent](https://developer.mozilla.org/en-US/docs/Web/API/MutationEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MutationEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MutationEvent>
**/
@:native("MutationEvent")
extern class MutationEvent extends Event
{
	static inline var MODIFICATION : Int = 1;
	static inline var ADDITION : Int = 2;
	static inline var REMOVAL : Int = 3;
	
	var relatedNode(default,null) : Node;
	var prevValue(default,null) : String;
	var newValue(default,null) : String;
	var attrName(default,null) : String;
	var attrChange(default,null) : Int;
	
	/** @throws DOMError */
	function initMutationEvent( type : String, ?canBubble : Bool = false, ?cancelable : Bool = false, ?relatedNode : Node, ?prevValue : String = "", ?newValue : String = "", ?attrName : String = "", ?attrChange : Int = 0 ) : Void;
}