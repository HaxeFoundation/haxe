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

package js;

import haxe.extern.EitherType;

@:native("Promise")
extern class Promise<T>
{
	@:overload(function<T>(promise : Promise<T>) : Promise<T> {})
	@:overload(function<T>(thenable : Thenable<T>) : Promise<T> {})
	static function resolve<T>( ?value : T ) : Promise<T>;

	static function reject<T>( ?value : Dynamic ) : Promise<T>;

	static function all( iterable : Array<Dynamic> ) : Promise<Array<Dynamic>>;

	static function race( iterable : Array<Dynamic> ) : Promise<Dynamic>;

	/** @throws DOMError */
	function new( init : (T -> Void) -> (Dynamic -> Void) -> Void ) : Void;

	function then<TOut>( fulfillCallback : Null<PromiseCallback<T, TOut>>, ?rejectCallback : EitherType<Dynamic -> Void, PromiseCallback<Dynamic, TOut>> ) : Promise<TOut>;

	@:native("catch")
	function catchError<TOut>( rejectCallback : EitherType<Dynamic -> Void, PromiseCallback<Dynamic, TOut>> ) : Promise<TOut>;
}

typedef PromiseCallback<T, TOut> = EitherType<T -> TOut, T -> Promise<TOut>>;

typedef Thenable<T> = {
	function then(resolve:T->Void, ?reject:Dynamic->Void):Void;
}
