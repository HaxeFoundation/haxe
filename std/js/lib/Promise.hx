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

package js.lib;

import haxe.extern.EitherType;

/**
	The Promise object represents the eventual completion (or failure) of an
	asynchronous operation and its resulting value.

	Documentation [Promise](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("Promise")
extern class Promise<T> {
	/**
		Returns a Promise object that is resolved with the given value. If the
		value is Thenable, the returned promise will "follow" that
		thenable, adopting its eventual state;
		otherwise the returned promise will be fulfilled with the value.
		Generally, when it's unknown when value is a promise or not,
		use `Promise.resolve(value)` instead and work with the return value as
		a promise.
	**/
	@:overload(function<T>(?value:T):Promise<T> {})
	static function resolve<T>(thenable:Thenable<T>):Promise<T>;

	/**
		Returns a Promise object that is rejected with the given reason.
	**/
	static function reject<T>(?reason:Dynamic):Promise<T>;

	/**
		Returns a promise that either fulfills when all of the promises in the
		iterable argument have fulfilled or rejects as soon as one of the
		promises in the iterable argument rejects. If the returned promise
		fulfills, it is fulfilled with an array of the values from the
		fulfilled promises in the same order as defined in the iterable.
		If the returned promise rejects, it is rejected with the reason from
		the first promise in the iterable that rejected. This method can be
		useful for aggregating results of multiple promises.
	**/
	@:overload(function(iterable:Array<Dynamic>):Promise<Array<Dynamic>> {})
	static function all<T>(iterable:Array<Promise<T>>):Promise<Array<T>>;

	/**
		Returns a promise that resolves after all of the given promises have either fulfilled or rejected,
		with an array of objects that each describes the outcome of each promise.

		It is typically used when you have multiple asynchronous tasks that are not dependent on one another
		to complete successfully, or you'd always like to know the result of each promise.

		In comparison, the Promise returned by `Promise.all` may be more appropriate if the tasks are dependent
		on each other / if you'd like to immediately reject upon any of them rejecting.
	**/
	@:overload(function(iterable:Array<Dynamic>):Promise<Array<PromiseSettleOutcome<Dynamic>>> {})
	static function allSettled<T>(iterable:Array<Promise<T>>):Promise<Array<PromiseSettleOutcome<T>>>;

	/**
		Returns a promise that fulfills or rejects as soon as one of the
		promises in the iterable fulfills or rejects, with the value or reason
		from that promise.
	**/
	@:overload(function(iterable:Array<Dynamic>):Promise<Dynamic> {})
	static function race<T>(iterable:Array<Promise<T>>):Promise<T>;

	/** @throws DOMError */
	function new(init:(resolve:(value:T) -> Void, reject:(reason:Dynamic) -> Void) -> Void):Void;

	/**
		Appends fulfillment and rejection handlers to the promise and returns a
		new promise resolving to the return value of the called handler, or to
		its original settled value if the promise was not handled
		(i.e. if the relevant handler onFulfilled or onRejected is not a function).
	**/
	function then<TOut>(onFulfilled:Null<PromiseHandler<T, TOut>>, ?onRejected:PromiseHandler<Dynamic, TOut>):Promise<TOut>;

	/**
		Appends a rejection handler callback to the promise, and returns a new
		promise resolving to the return value of the callback if it is called,
		or to its original fulfillment value if the promise is instead fulfilled.
	**/
	@:native("catch")
	@:overload(function<TOut>(onRejected:PromiseHandler<Dynamic, TOut>):Promise<EitherType<T, TOut>> {})
	function catchError(onRejected:PromiseHandler<Dynamic, T>):Promise<T>;

	/**
		Returns a Promise. When the promise is settled, i.e either fulfilled or rejected,
		the specified callback function is executed. This provides a way for code to be run
		whether the promise was fulfilled successfully or rejected once the Promise has been dealt with.
	**/
	function finally(onFinally:() -> Void):Promise<T>;
}

/**
	Handler type for the Promise object.
**/
abstract PromiseHandler<T, TOut>(T->Dynamic) // T->Dynamic, so the compiler always knows the type of the argument and can infer it for then/catch callbacks
	from T->Promise<TOut> // support Promise explicitly as it doesn't work transitively through Thenable at the moment
	from T->Thenable<TOut> // although the checking order seems to be reversed at the moment, see https://github.com/HaxeFoundation/haxe/issues/7656
	from T->TOut // order is important, because Promise<TOut> return must have priority
{}

/**
	A value with a `then` method.
**/
@:forward
@:transitive
abstract Thenable<T>(ThenableStruct<T>)
	from ThenableStruct<T> {} // abstract wrapping prevents compiler hanging, see https://github.com/HaxeFoundation/haxe/issues/5785

typedef ThenableStruct<T> = {
	function then<TOut>(onFulfilled:Null<PromiseHandler<T, TOut>>, ?onRejected:PromiseHandler<Dynamic, TOut>):Thenable<TOut>;
}

typedef PromiseSettleOutcome<T> = {
	var status:PromiseSettleStatus;
	var ?value:T;
	var ?reason:Dynamic;
}

enum abstract PromiseSettleStatus(String) to String {
	var Fulfilled = "fulfilled";
	var Rejected = "rejected";
}
