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

// This file is generated from mozilla\Plugin.webidl. Do not edit!

package js.html;

/**
	The `Plugin` interface provides information about a browser plugin.

	Documentation [Plugin](https://developer.mozilla.org/en-US/docs/Web/API/Plugin) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Plugin$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Plugin>
**/
@:native("Plugin")
extern class Plugin implements ArrayAccess<MimeType>
{
	
	/**
		A human readable description of the plugin.
	**/
	var description(default,null) : String;
	
	/**
		The filename of the plugin file.
	**/
	var filename(default,null) : String;
	
	/**
		The plugin's version number string.
	**/
	var version(default,null) : String;
	
	/**
		The name of the plugin.
	**/
	var name(default,null) : String;
	var length(default,null) : Int;
	
	function item( index : Int ) : MimeType;
	function namedItem( name : String ) : MimeType;
}