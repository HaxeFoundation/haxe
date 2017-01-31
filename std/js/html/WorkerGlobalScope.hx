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

// This file is generated from mozilla\WorkerGlobalScope.webidl. Do not edit!

package js.html;

/**
	The `WorkerGlobalScope` interface of the Web Workers API is an interface representing the scope of any worker. Workers have no browsing context; this scope contains the information usually conveyed by `Window` objects — in this case event handlers, the console or the associated `WorkerNavigator` object. Each `WorkerGlobalScope` has its own event loop.

	Documentation [WorkerGlobalScope](https://developer.mozilla.org/en-US/docs/Web/API/WorkerGlobalScope) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/WorkerGlobalScope$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/WorkerGlobalScope>
**/
@:native("WorkerGlobalScope")
extern class WorkerGlobalScope extends EventTarget
{
	
	/**
		Returns a reference to the `WorkerGlobalScope` itself. Most of the time it is a specific scope like `DedicatedWorkerGlobalScope`,  `SharedWorkerGlobalScope` or `ServiceWorkerGlobalScope`.
	**/
	var self(default,null) : WorkerGlobalScope;
	
	/**
		Returns the `Console` associated with the worker.
	**/
	var console(default,null) : Console;
	
	/**
		Returns the `WorkerLocation` associated with the worker. It is a specific location object, mostly a subset of the `Location` for browsing scopes, but adapted to workers.
	**/
	var location(default,null) : WorkerLocation;
	
	/**
		Is an `EventHandler` representing the code to be called when the `error` event is raised.
	**/
	var onerror : haxe.extern.EitherType<Event,String> -> String -> Int -> Int -> Dynamic -> Bool;
	
	/**
		Is an `EventHandler` representing the code to be called when the `offline` event is raised.
	**/
	var onoffline : haxe.Constraints.Function;
	
	/**
		Is an `EventHandler` representing the code to be called when the `online` event is raised.
	**/
	var ononline : haxe.Constraints.Function;
	
	/**
		Returns the `WorkerNavigator` associated with the worker. It is a specific navigator object, mostly a subset of the `Navigator` for browsing scopes, but adapted to workers.
	**/
	var navigator(default,null) : WorkerNavigator;
	
	/**
		Is an `EventHandler` representing the code to be called when the `close` event is raised.
	**/
	var onclose : haxe.Constraints.Function;
	
	/**
		Returns the `Performance` associated with the worker. It is a regular performance object, except that only a subset of its property and methods are available to workers.
	**/
	var performance(default,null) : Performance;
	var indexedDB(default,null) : js.html.idb.Factory;
	
	/** @throws DOMError */
	
	/**
		Discards any tasks queued in the `WorkerGlobalScope`'s event loop, effectively closing this particular scope.
	**/
	function close() : Void;
	/** @throws DOMError */
	
	/**
		Imports one or more scripts into the worker's scope. You can specify as many as you'd like, separated by commas. For example:` importScripts('foo.js', 'bar.js');`
	**/
	function importScripts( urls : haxe.extern.Rest<String> ) : Void;
	
	/**
		Allows you to write a message to stdout — i.e. in your terminal. This is the same as Firefox's `window.dump`, but for workers.
	**/
	function dump( ?str : String ) : Void;
	/** @throws DOMError */
	function fetch( input : haxe.extern.EitherType<Request,String>, ?init : RequestInit ) : Promise<Response>;
	/** @throws DOMError */
	@:overload( function( aImage : haxe.extern.EitherType<ImageElement,haxe.extern.EitherType<VideoElement,haxe.extern.EitherType<CanvasElement,haxe.extern.EitherType<Blob,haxe.extern.EitherType<ImageData,haxe.extern.EitherType<CanvasRenderingContext2D,ImageBitmap>>>>>> ) : Promise<ImageBitmap> {} )
	function createImageBitmap( aImage : haxe.extern.EitherType<ImageElement,haxe.extern.EitherType<VideoElement,haxe.extern.EitherType<CanvasElement,haxe.extern.EitherType<Blob,haxe.extern.EitherType<ImageData,haxe.extern.EitherType<CanvasRenderingContext2D,ImageBitmap>>>>>>, aSx : Int, aSy : Int, aSw : Int, aSh : Int ) : Promise<ImageBitmap>;
	/** @throws DOMError */
	function btoa( btoa : String ) : String;
	/** @throws DOMError */
	function atob( atob : String ) : String;
	/** @throws DOMError */
	@:overload( function( handler : haxe.Constraints.Function, ?timeout : Int = 0, arguments : haxe.extern.Rest<Dynamic> ) : Int {} )
	function setTimeout( handler : String, ?timeout : Int = 0, unused : haxe.extern.Rest<Dynamic> ) : Int;
	function clearTimeout( ?handle : Int = 0 ) : Void;
	/** @throws DOMError */
	@:overload( function( handler : haxe.Constraints.Function, ?timeout : Int, arguments : haxe.extern.Rest<Dynamic> ) : Int {} )
	function setInterval( handler : String, ?timeout : Int, unused : haxe.extern.Rest<Dynamic> ) : Int;
	function clearInterval( ?handle : Int = 0 ) : Void;
}