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

/** DOM <code>TextArea</code> objects expose the <a title="http://dev.w3.org/html5/spec/the-button-element.html#the-textarea-element" class=" external" rel="external" href="http://dev.w3.org/html5/spec/the-button-element.html#the-textarea-element" target="_blank">HTMLTextAreaElement</a> (or 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span> <code><a title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-24874179" class="external" rel="external" href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-24874179" target="_blank">HTMLTextAreaElement</a></code>) interface, which provides special properties and methods (beyond the regular <a title="en/DOM/element" rel="internal" href="https://developer.mozilla.org/en/DOM/element">element</a> object interface they also have available to them by inheritance) for manipulating the layout and presentation of <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/textarea">&lt;textarea&gt;</a></code>
 elements.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLTextAreaElement">MDN</a>. */
@:native("HTMLTextAreaElement")
extern class TextAreaElement extends Element
{
	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/textarea#attr-autofocus">autofocus</a></code>
 HTML&nbsp;attribute, indicating that the control should have input focus when the page loads */
	var autofocus : Bool;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/textarea#attr-cols">cols</a></code>
 HTML attribute, indicating the visible width of the text area. */
	var cols : Int;

	/** The control's default value, which behaves like the <strong><a title="en/DOM/element.textContent" rel="internal" href="https://developer.mozilla.org/En/DOM/Node.textContent">textContent</a></strong> property. */
	var defaultValue : String;

	var dirName : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/textarea#attr-disabled">disabled</a></code>
 HTML attribute, indicating that the control is not available for interaction. */
	var disabled : Bool;

	/** <p>The containing form element, if this element is in a form. If this element is not contained in a form element:</p> <ul> <li>
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span> this can be the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form#attr-id">id</a></code>
 attribute of any <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form">&lt;form&gt;</a></code>
 element in the same document.</li> <li>
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span> this must be <code>null</code>.</li> </ul> */
	var form (default,null) : FormElement;

	/** A list of <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/label">&lt;label&gt;</a></code>
 elements that are labels for this element. */
	var labels (default,null) : NodeList;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/textarea#attr-maxlength">maxlength</a></code>
 HTML&nbsp;attribute, indicating the maximum number of characters the user can enter. This constraint is evaluated only when the value changes. Setter throws DOMException. */
	var maxLength : Int;

	/** Reflects 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/textarea#attr-name">name</a></code>
 HTML attribute, containing the name of the control. */
	var name : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/textarea#attr-placeholder">placeholder</a></code>
 HTML attribute, containing a hint to the user about what to enter in the control. */
	var placeholder : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/textarea#attr-readonly">readonly</a></code>
 HTML attribute, indicating that the user cannot modify the value of the control. */
	var readOnly : Bool;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/textarea#attr-required">required</a></code>
 HTML attribute, indicating that the user must specify a value before submitting the form. */
	var required : Bool;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/textarea#attr-rows">rows</a></code>
 HTML attribute, indicating the number of visible text lines for the control. */
	var rows : Int;

	/** The direction in which selection occurred. This is "forward" if selection was performed in the start-to-end direction of the current locale, or "backward" for the opposite direction. This can also be "none"&nbsp;if the direction is unknown." */
	var selectionDirection : String;

	/** The index of the end of selected text. 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span> If no text is selected, contains the index of the character that follows the input cursor. On being set, the control behaves as if <strong>setSelectionRange</strong>() had been called with this as the second argument, and <strong>selectionStart</strong> as the first argument. */
	var selectionEnd : Int;

	/** The index of the beginning of selected text. 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span> If no text is selected, contains the index of the character that follows the input cursor. On being set, the control behaves as if <strong>setSelectionRange</strong>() had been called with this as the first argument, and <strong>selectionEnd</strong> as the second argument. */
	var selectionStart : Int;

	/** The codepoint length of the control's value. */
	var textLength (default,null) : Int;

	/** The string <code>textarea</code>. */
	var type (default,null) : String;

	/** A localized message that describes the validation constraints that the control does not satisfy (if any). This is the empty string if the control is not a candidate for constraint validation (<strong>willValidate</strong> is false), or it satisfies its constraints. */
	var validationMessage (default,null) : String;

	/** The validity states that this element is in. */
	var validity (default,null) : ValidityState;

	/** The raw value contained in the control. */
	var value : String;

	/** Indicates whether the element is a candidate for constraint validation. It is false if any conditions bar it from constraint validation. */
	var willValidate (default,null) : Bool;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/textarea#attr-wrap">wrap</a></code>
 HTML attribute, indicating how the control wraps text. */
	var wrap : String;

	function checkValidity() : Bool;

	function select() : Void;

	function setCustomValidity( error : String ) : Void;

	/** Throws DOMException. */
	@:overload( function( replacement : String ) :Void {} )
	function setRangeText( replacement : String, start : Int, end : Int, selectionMode : String ) : Void;

	function setSelectionRange( start : Int, end : Int, ?direction : String ) : Void;

}
