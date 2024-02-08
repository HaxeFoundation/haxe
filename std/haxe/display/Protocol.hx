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

package haxe.display;

import haxe.display.Position;

@:publicFields
class Methods {
	/**
		The initialize request is sent from the client to Haxe to determine the capabilities.
	**/
	static inline var Initialize = new HaxeRequestMethod<InitializeParams, InitializeResult>("initialize");

	static inline var ResetCache = new HaxeRequestMethod<ResetCacheParams, ResetCacheResult>("server/resetCache");
}

typedef ResetCacheParams = {}

typedef ResetCacheResult = {
	final success:Bool;
}

/* Initialize */
typedef InitializeParams = {
	final ?supportsResolve:Bool;

	/** dot paths to exclude from readClassPaths / toplevel completion **/
	final ?exclude:Array<String>;

	/** The maximum number of completion items to return **/
	final ?maxCompletionItems:Int;
}

/**
	Represents a semantic version, see https://semver.org/.
**/
typedef Version = {
	final major:Int;
	final minor:Int;
	final patch:Int;
	final ?pre:String;
	final ?build:String;
}

typedef InitializeResult = Response<{
	final protocolVersion:Version;
	final haxeVersion:Version;
	final methods:Array<String>;
}>;

/* general protocol types */
typedef Timer = {
	final name:String;
	final time:Float;
	final ?path:String;
	final ?info:String;
	final ?calls:Int;
	final ?percentTotal:Float;
	final ?percentParent:Float;
	final ?children:Array<Timer>;
}

typedef Response<T> = {
	final ?result:T;

	/** UNIX timestamp at the moment the data was sent. **/
	final ?timestamp:Float;

	/** Only sent if `--times` is enabled. **/
	final ?timers:Timer;
}

typedef FileParams = {
	var file:FsPath;
}

abstract HaxeRequestMethod<TParams, TResponse>(String) to String {
	public inline function new(method:String)
		this = method;
}

abstract HaxeNotificationMethod<TParams>(String) to String {
	public inline function new(method:String)
		this = method;
}

typedef HaxeResponseErrorData = Array<{
	var severity:HaxeResponseErrorSeverity;
	var ?location:Location;
	var message:String;
}>;

enum abstract HaxeResponseErrorSeverity(Int) {
	var Error = 1;
	var Warning;
	var Hint;
}

enum NoData {}
