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

// This file is generated from mozilla\DragEvent.webidl. Do not edit!

package js.html;

/**
	The `DragEvent` interface is a `DOM event` that represents a drag and drop interaction. The user initiates a drag by placing a pointer device (such as a mouse) on the touch surface and then dragging the pointer to a new location (such as another DOM element). Applications are free to interpret a drag and drop interaction in an application-specific way.

	Documentation [DragEvent](https://developer.mozilla.org/en-US/docs/Web/API/DragEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DragEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DragEvent>
**/
@:native("DragEvent")
extern class DragEvent extends MouseEvent
{
	var dataTransfer(default,null) : DataTransfer;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : DragEventInit ) : Void;
	function initDragEvent( type : String, canBubble : Bool, cancelable : Bool, aView : Window, aDetail : Int, aScreenX : Int, aScreenY : Int, aClientX : Int, aClientY : Int, aCtrlKey : Bool, aAltKey : Bool, aShiftKey : Bool, aMetaKey : Bool, aButton : Int, aRelatedTarget : EventTarget, aDataTransfer : DataTransfer ) : Void;
}