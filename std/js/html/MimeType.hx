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

// This file is generated from mozilla\MimeType.webidl. Do not edit!

package js.html;

/**
	The `MimeType` interface provides contains information about a MIME type associated with a particular plugin. `NavigatorPlugins.mimeTypes` returns an array of this object.

	Documentation [MimeType](https://developer.mozilla.org/en-US/docs/Web/API/MimeType) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MimeType$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MimeType>
**/
@:native("MimeType")
extern class MimeType {
	
	/**
		Returns a description of the associated plugin or an empty string if there is none.
	**/
	var description(default,null) : String;
	
	/**
		Returns an instance of `Plugin` containing information about the plugin itself.
	**/
	var enabledPlugin(default,null) : Plugin;
	
	/**
		A string containing valid file extensions for the data displayed by the plugin, or an empty string if an extension is not valid for the particular module. For example, a browser's content decryption module may appear in the plugin list, but support more file extenions than can be anticipated. It might therefore return an empty string.
	**/
	var suffixes(default,null) : String;
	
	/**
		Returns the MIME type of the associated plugin.
	**/
	var type(default,null) : String;
	
}