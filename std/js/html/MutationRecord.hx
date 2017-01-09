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

// This file is generated from mozilla\MutationObserver.webidl. Do not edit!

package js.html;

/**
	A `MutationRecord` represents an individual DOM mutation. It is the object that is passed to `MutationObserver`'s callback.

	Documentation [MutationRecord](https://developer.mozilla.org/en-US/docs/Web/API/MutationRecord) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MutationRecord$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MutationRecord>
**/
@:native("MutationRecord")
extern class MutationRecord
{
	var type(default,null) : String;
	var target(default,null) : Node;
	var addedNodes(default,null) : NodeList;
	var removedNodes(default,null) : NodeList;
	var previousSibling(default,null) : Node;
	var nextSibling(default,null) : Node;
	var attributeName(default,null) : String;
	var attributeNamespace(default,null) : String;
	var oldValue(default,null) : String;
	
}