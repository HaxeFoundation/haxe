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
package js.swfobject;

import js.html.*;
import haxe.extern.*;

typedef SWFObjectEvent = {
	/**
		Boolean to indicate whether the creation of a Flash plugin-in <object> DOM was successful or not
	*/
	var success:Bool;
	/**
		String indicating the ID used in swfobject.registerObject
	*/
	var id:String;
	/**
		HTML object element reference (null when success==false)
	*/
	@:optional var ref:Element;
}

/**
	For detecting the Adobe Flash Player plugin and embedding Flash (swf) files.
*/
@:native("swfobject")
extern class SWFObject {
	/**
		Embed Flash content and alternative content using standards compliant markup (the nested-objects method with proprietary Internet Explorer conditional comments), and use JavaScript to resolve the issues that markup alone cannot solve (also called static publishing).
	*/
	static function registerObject(objectIdStr:String, swfVersionStr:String, xiSwfUrlStr:String, callbackFn:SWFObjectEvent->Void):Void;
	/**
		NOTE: For static publishing only (or: when using swfobject.registerObject() only)

		Returns the active object element. One of the side-effects of the nested-objects approach is that per SWF there are two object elements available in the HTML code, however you can only use one id or name attribute, because these have to be unique within a web page.
	*/
	static function getObjectById(objectIdStr:String):Element;
	/**
		Insert alternative content using standards compliant markup and embed Flash content with unobtrusive JavaScript (also called dynamic publishing).
	*/
	static function embedSWF(swfUrlStr:String, replaceElemIdStr:String, widthStr:String, heightStr:String, swfVersionStr:String, xiSwfUrlStr:String, flashvarsObj:Dynamic, parObj:Dynamic, attObj:Dynamic, callbackFn:SWFObjectEvent->Void):Void;
	/**
		Disable SWFObject's default show/hide behavior
	*/
	static function switchOffAutoHideShow():Void;
	static function enableUriEncoding(bool:Bool):Void;
	static var ua:{
		/**
			a Boolean whether or not W3C DOM methods are supported
		*/
		var w3:Bool;
		/**
			an Array that contains the major, minor and release version number of the Flash Player
		*/
		var pv:Array<Int>;
		/**
			the Webkit version or false if not Webkit
		*/
		var wk:EitherType<Float, Bool>;
		/**
			a Boolean to indicate whether a visitor uses Internet Explorer on Windows or not
		*/
		var ie:Bool;
		/**
			a Boolean to indicate whether a visitor uses Mac OS or not
		*/
		var mac:Bool;
	};
	/**
		Returns a JavaScript object containing the major version (major:Number), minor version (minor:Number) and release version (release:Number) of an installed Flash player. Please note that while Flash version numbers normally consist of major.minor.release.build, SWFObject only looks at the first 3 numbers, so both "WIN 9,0,18,0" (IE) or "Shockwave Flash 9 r18" (all other browsers) will translate to "9.0.18".
	*/
	static function getFlashPlayerVersion():{ major:Int, minor:Int, release:Int };
	/**
		Returns a Boolean to indicate whether or not a specific minimum version of the Flash plugin is installed. Please note that while Flash version numbers normally consist of major.minor.release.build, SWFObject only looks at the first 3 numbers, so both "WIN 9,0,18,0" (IE) or "Shockwave Flash 9 r18" (all other browsers) will translate to "9.0.18".
	*/
	static function hasFlashPlayerVersion(versionStr:String):Bool;
	/**
		Exposes SWFObject's internal cross-browser method to create a SWF.
	*/
	static function createSWF(attObj:Dynamic, parObj:Dynamic, replaceElemIdStr:String):Element;
	/**
		Enables developers to reuse SWFObject's internal methods to create their custom Express Install and activate it via the API
	*/
	static function showExpressInstall(att:Dynamic, par:Dynamic, replaceElemIdStr:String, callbackFn:SWFObjectEvent->Void):Bool;
	/**
		Removes a SWF from your web page. Is especially built to safely (only remove a SWF after it has been loaded, to avoid broken references) and completely (nullify references to avoid memory leaks) remove a SWF in Internet Explorer.
	*/
	static function removeSWF(objElemIdStr:String):Void;
	/**
		Exposes SWFObject's internal cross-browser method to create a dynamic stylesheet. It's most important feature is that it can be called before the DOM of a page is loaded.
	*/
	static function createCSS(selStr:String, declStr:String, ?mediaStr:String, ?newStyleBoolean:Bool):Void;
	/**
		A cross-browser method to execute a function as soon as the DOM of a web page is available. This method is supported by Gecko based browsers - like Firefox -, IE, Opera9+, and, Safari. For all other browsers the method will fall back to the addLoadEvent method. 
	*/
	static function addDomLoadEvent(fn:Void->Void):Void;
	/**
		A safe (it doesn't overwrite existing onload events) cross-browser method to execute a function on the window onload event (which will fire as soon as a web page including all of its assets are loaded).
	*/
	static function addLoadEvent(fn:Void->Void):Void;
	/**
		Utility function that returns the value of the paramStr parameter in the URL query string or hash string. Similar functionality was already available in SWFObject 1.5 and lower.
	*/
	static function getQueryParamValue(param:String):String;
	/**
		swfobject version
	*/
	static var version:String;
}