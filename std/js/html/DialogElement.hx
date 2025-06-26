/*
 * Copyright (C)2005-2022 Haxe Foundation
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

package js.html;

/**
	The `HTMLDialogElement` interface provides methods to manipulate `<dialog>` elements.
	It inherits properties and methods from the `HTMLElement` interface.

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLDialogElement>
**/
@:native("HTMLDialogElement")
extern class DialogElement extends Element {

	/**
		A `Boolean` reflecting the open HTML attribute, indicating whether the dialog is available for interaction.
	**/
	var open: Bool;

	/**
		A `DOMString` that sets or returns the return value for the dialog.
	**/
	var returnValue: String;

	/**
		Closes the dialog.
		An optional `DOMString` may be passed as an argument, updating the `returnValue` of the the dialog.
	**/
	function close(?returnValue: String): Void;

	/**
		Displays the dialog modelessly, i.e. still allowing interaction with content outside of the dialog.
	**/
	function show(): Void;

	/**
		Displays the dialog as a modal, over the top of any other dialogs that might be present.
		Interaction outside the dialog is blocked.
	**/
	function showModal(): Void;
}
