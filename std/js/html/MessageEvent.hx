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

// This file is generated from mozilla\MessageEvent.webidl. Do not edit!

package js.html;

/**
	The `MessageEvent` interface represents a message received by a target object.

	Documentation [MessageEvent](https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent>
**/
@:native("MessageEvent")
extern class MessageEvent extends Event {
	
	/**
		The data sent by the message emitter.
	**/
	var data(default,null) : Dynamic;
	
	/**
		A `USVString` representing the origin of the message emitter.
	**/
	var origin(default,null) : String;
	
	/**
		A `DOMString` representing a unique ID for the event.
	**/
	var lastEventId(default,null) : String;
	
	/**
		A `MessageEventSource` (which can be a `WindowProxy`, `MessagePort`, or `ServiceWorker` object) representing the message emitter.
	**/
	var source(default,null) : haxe.extern.EitherType<Window,haxe.extern.EitherType<MessagePort,ServiceWorker>>;
	
	/**
		An array of `MessagePort` objects representing the ports associated with the channel the message is being sent through (where appropriate, e.g. in channel messaging or when sending a message to a shared worker).
	**/
	var ports(default,null) : Array<MessagePort>;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : MessageEventInit ) : Void;
	
	/**
		Initializes a message event. Do not use this anymore — use the `MessageEvent.MessageEvent` constructor instead.
	**/
	@:overload( function( type : String, bubbles : Bool = false, cancelable : Bool = false, ?data : Dynamic, origin : String = "", lastEventId : String = "", ?source : MessagePort, ?ports : Array<MessagePort>) : Void {} )
	@:overload( function( type : String, bubbles : Bool = false, cancelable : Bool = false, ?data : Dynamic, origin : String = "", lastEventId : String = "", ?source : ServiceWorker, ?ports : Array<MessagePort>) : Void {} )
	function initMessageEvent( type : String, bubbles : Bool = false, cancelable : Bool = false, ?data : Dynamic, origin : String = "", lastEventId : String = "", ?source : Window, ?ports : Array<MessagePort> ) : Void;
}