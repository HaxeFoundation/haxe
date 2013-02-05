/*
 * Copyright (C)2005-2013 Haxe Foundation
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

// This file is generated, do not edit!
package js.html;

/** The HTML <em>progress</em> (<code>&lt;progress&gt;</code>) element is used to view the completion progress of a task. While the specifics of how it's displayed is left up to the browser developer, it's typically displayed as a progress bar.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/HTML/Element/progress">MDN</a>. */
@:native("HTMLProgressElement")
extern class ProgressElement extends Element
{
	var labels(default,null) : NodeList;

	/** This attribute describes how much work the task indicated by the <code>progress</code> element requires. Setter throws DOMException. */
	var max : Float;

	var position(default,null) : Float;

	/** <dl><dd>This attribute specifies how much of the task that has been completed. If there is no <code>value</code> attribute, the progress bar is indeterminate; this indicates that an activity is ongoing with no indication of how long it is expected to take.</dd>
</dl>
<p>You can use the <code><a rel="custom" href="https://developer.mozilla.org/en/CSS/orient">orient</a></code>
 property to specify whether the progress bar should be rendered horizontally (the default) or vertically. The <code><a rel="custom" href="https://developer.mozilla.org/en/CSS/%3Aindeterminate">:indeterminate</a></code>
 pseudo-class can be used to match against indeterminate progress bars.</p> Setter throws DOMException. */
	var value : Float;

}
