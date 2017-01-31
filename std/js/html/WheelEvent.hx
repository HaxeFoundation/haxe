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

// This file is generated from mozilla\WheelEvent.webidl. Do not edit!

package js.html;

/**
	The `WheelEvent` interface represents events that occur due to the user moving a mouse wheel or similar input device.

	Documentation [WheelEvent](https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent>
**/
@:native("WheelEvent")
extern class WheelEvent extends MouseEvent
{
	static inline var DOM_DELTA_PIXEL : Int = 0;
	static inline var DOM_DELTA_LINE : Int = 1;
	static inline var DOM_DELTA_PAGE : Int = 2;
	
	
	/**
		Returns a `double` representing the horizontal scroll amount.
	**/
	var deltaX(default,null) : Float;
	
	/**
		Returns a `double` representing the vertical scroll amount.
	**/
	var deltaY(default,null) : Float;
	
	/**
		Returns a `double` representing the scroll amount for the z-axis.
	**/
	var deltaZ(default,null) : Float;
	
	/**
		Returns an <code>unsigned long</code> representing the unit of the delta values scroll amount. Permitted values are:
		 <table class="standard-table">
		  
		   <tr>
		    <td class="header">Constant</td>
		    <td class="header">Value</td>
		    <td class="header">Description</td>
		   </tr>
		   <tr>
		    <td><code>DOM_DELTA_PIXEL</code></td>
		    <td><code>0x00</code></td>
		    <td>The delta values are specified in pixels.</td>
		   </tr>
		   <tr>
		    <td><code>DOM_DELTA_LINE</code></td>
		    <td><code>0x01</code></td>
		    <td>The delta values are specified in lines.</td>
		   </tr>
		   <tr>
		    <td><code>DOM_DELTA_PAGE</code></td>
		    <td><code>0x02</code></td>
		    <td>The delta values are specified in pages.</td>
		   </tr>
		  
		 </table>
		 
	**/
	var deltaMode(default,null) : Int;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : WheelEventInit ) : Void;
}