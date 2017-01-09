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

// This file is generated from mozilla\WorkerNavigator.webidl. Do not edit!

package js.html;

/**
	The `WorkerNavigator` interface represents a subset of the `Navigator` interface allowed to be accessed from a `Worker`. Such an object is initialized for each worker and is available via the `WorkerGlobalScope.navigator` property obtained by calling `window.self.navigator`.

	Documentation [WorkerNavigator](https://developer.mozilla.org/en-US/docs/Web/API/WorkerNavigator) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/WorkerNavigator$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/WorkerNavigator>
**/
@:native("WorkerNavigator")
extern class WorkerNavigator
{
	var hardwareConcurrency(default,null) : Int;
	var appCodeName(default,null) : String;
	var appName(default,null) : String;
	var appVersion(default,null) : String;
	var platform(default,null) : String;
	var userAgent(default,null) : String;
	var product(default,null) : String;
	var language(default,null) : String;
	var languages(default,null) : Array<String>;
	var onLine(default,null) : Bool;
	
	function taintEnabled() : Bool;
}