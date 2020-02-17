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

// This file is generated from mozilla\MutationObserver.webidl. Do not edit!

package js.html;

/**
	The `MutationObserverInit` dictionary describes the configuration of a mutation observer. As such, it's primarily used as the type of the `options` parameter on the `MutationObserver.observe()` method.

	Documentation [MutationObserverInit](https://developer.mozilla.org/en-US/docs/Web/API/MutationObserverInit) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MutationObserverInit$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MutationObserverInit>
**/
typedef MutationObserverInit = {
	
	/**
		An array of specific attribute names to be monitored. If this property isn't included, changes to all attributes cause mutation notifications. No default value.
	**/
	var ?attributeFilter : Array<String>;
	
	/**
		Set to `true` to record the previous value of any attribute that changes when monitoring the node or nodes for attribute changes; see `/en-US/docs/Web/API/MutationObserver` for details on watching for attribute changes and value recording. No default value.
	**/
	var ?attributeOldValue : Bool;
	
	/**
		Set to `true` to watch for changes to the value of attributes on the node or nodes being monitored. The default value is `false`.
	**/
	var ?attributes : Bool;
	
	/**
		Set to `true` to monitor the specified target node or subtree for changes to the character data contained within the node or nodes. No default value.
	**/
	var ?characterData : Bool;
	
	/**
		Set to `true` to record the previous value of a node's text whenever the text changes on nodes being monitored. For details and an example, see `/en-US/docs/Web/API/MutationObserver`. No default value.
	**/
	var ?characterDataOldValue : Bool;
	
	/**
		Set to `true` to monitor the target node (and, if `subtree` is `true`, its descendants) for the addition or removal of new child nodes. The default is `false`.
	**/
	var ?childList : Bool;
	
	/**
		Set to `true` to extend monitoring to the entire subtree of nodes rooted at `target`. All of the other `MutationObserverInit` properties are then extended to all of the nodes in the subtree instead of applying solely to the `target` node. The default value is `false`.
	**/
	var ?subtree : Bool;
}