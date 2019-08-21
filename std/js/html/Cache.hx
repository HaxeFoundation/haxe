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

// This file is generated from mozilla\Cache.webidl. Do not edit!

package js.html;

import js.lib.Promise;

/**
	The `Cache` interface provides a storage mechanism for `Request` / `Response` object pairs that are cached, for example as part of the `ServiceWorker` life cycle. Note that the `Cache` interface is exposed to windowed scopes as well as workers. You don't have to use it in conjunction with service workers, even though it is defined in the service worker spec.

	Documentation [Cache](https://developer.mozilla.org/en-US/docs/Web/API/Cache) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Cache$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Cache>
**/
@:native("Cache")
extern class Cache {
	@:overload( function( request : String, ?options : CacheQueryOptions) : Promise<Response> {} )
	function match( request : Request, ?options : CacheQueryOptions ) : Promise<Response>;
	@:overload( function( ?request : String, ?options : CacheQueryOptions) : Promise<Array<Response>> {} )
	function matchAll( ?request : Request, ?options : CacheQueryOptions ) : Promise<Array<Response>>;
	@:overload( function( request : String) : Promise<Void> {} )
	function add( request : Request ) : Promise<Void>;
	function addAll( requests : Array<haxe.extern.EitherType<Request,String>> ) : Promise<Void>;
	@:overload( function( request : String, response : Response) : Promise<Void> {} )
	function put( request : Request, response : Response ) : Promise<Void>;
	@:overload( function( request : String, ?options : CacheQueryOptions) : Promise<Bool> {} )
	function delete( request : Request, ?options : CacheQueryOptions ) : Promise<Bool>;
	@:overload( function( ?request : String, ?options : CacheQueryOptions) : Promise<Array<Request>> {} )
	function keys( ?request : Request, ?options : CacheQueryOptions ) : Promise<Array<Request>>;
}