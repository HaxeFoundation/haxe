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

// This file is generated from mozilla\ExtendableMessageEvent.webidl. Do not edit!

package js.html;

/**
	The `ExtendableMessageEvent` interface of the `ServiceWorker API` represents the event object of a `message` event fired on a service worker (when a channel message is received on the `ServiceWorkerGlobalScope` from another context) — extends the lifetime of such events.

	Documentation [ExtendableMessageEvent](https://developer.mozilla.org/en-US/docs/Web/API/ExtendableMessageEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/ExtendableMessageEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/ExtendableMessageEvent>
**/
@:native("ExtendableMessageEvent")
extern class ExtendableMessageEvent extends ExtendableEvent
{
	
	/**
		Returns the event's data. It can be any data type.
	**/
	var data(default,null) : Dynamic;
	
	/**
		Returns the origin of the `ServiceWorkerClient` that sent the message
	**/
	var origin(default,null) : String;
	
	/**
		Represents, in server-sent events, the last event ID of the event source.
	**/
	var lastEventId(default,null) : String;
	
	/**
		Returns a reference to the service worker that sent the message.
	**/
	var source(default,null) : haxe.extern.EitherType<Client,haxe.extern.EitherType<ServiceWorker,MessagePort>>;
	
	/**
		Returns the array containing the `MessagePort` objects representing the ports of the associated message channel.
	**/
	var ports(default,null) : MessagePortList;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : ExtendableMessageEventInit ) : Void;
}