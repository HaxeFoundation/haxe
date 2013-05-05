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

/** DOM <code>Input</code> objects expose the <a title="http://dev.w3.org/html5/spec/the-input-element.html#htmlinputelement" class=" external" rel="external" href="http://dev.w3.org/html5/spec/the-input-element.html#htmlinputelement" target="_blank">HTMLInputElement</a> (or 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span> <a class=" external" rel="external" href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-6043025" title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-6043025" target="_blank"><code>HTMLInputElement</code></a>) interface, which provides special properties and methods (beyond the regular <a rel="internal" href="/api/js/html/Element">element</a> object interface they also have available to them by inheritance) for manipulating the layout and presentation of input elements.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLInputElement">MDN</a>. */
@:native("HTMLInputElement")
extern class InputElement extends Element
{
	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-accept">accept</a></code>
 HTML&nbsp;attribute, containing comma-separated list of file types accepted by the server when <strong>type</strong> is <code>file</code>. */
	var accept : String;

	/** Alignment of the element.

<span title="">Obsolete</span>&nbsp;in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span> */
	var align : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-alt">alt</a></code>
&nbsp;HTML attribute, containing alternative text to use when <strong>type</strong> is <code>image.</code> */
	var alt : String;

	/** Reflects the {{htmlattrxref("autocomplete", "input)}} HTML attribute, indicating whether the value of the control can be automatically completed by the browser. Ignored if the value of the <strong>type</strong> attribute is <span>hidden</span>, <span>checkbox</span>, <span>radio</span>, <span>file</span>, or a button type (<span>button</span>, <span>submit</span>, <span>reset</span>, <span>image</span>). Possible values are: <ul> <li><span>off</span>: The user must explicitly enter a value into this field for every use, or the document provides its own auto-completion method; the browser does not automatically complete the entry.</li> <li><span>on</span>: The browser can automatically complete the value based on values that the user has entered during previous uses.</li> </ul> */
	var autocomplete : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-autofocus">autofocus</a></code>
 HTML&nbsp;attribute, which specifies that a form control should have input focus when the page loads, unless the user overrides it, for example by typing in a different control. Only one form element in a document can have the <strong>autofocus</strong> attribute. It cannot be applied if the <strong>type</strong> attribute is set to <code>hidden</code> (that is, you cannot automatically set focus to a hidden control). */
	var autofocus : Bool;

	/** The current state of the element when <strong>type</strong> is <code>checkbox</code> or <code>radio</code>. */
	var checked : Bool;

	/** The default state of a radio button or checkbox as originally specified in HTML that created this object. */
	var defaultChecked : Bool;

	/** The default value as originally specified in HTML that created this object. */
	var defaultValue : String;

	var dirName : String;

	var directory : Bool;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-disabled">disabled</a></code>
 HTML attribute, indicating that the control is not available for interaction. */
	var disabled : Bool;

	var entries(default,null) : js.html.fs.EntryArray;

	/** A list of selected files. */
	var files : FileList;

	/** <p>The containing form element, if this element is in a form. If this element is not contained in a form element:</p> <ul> <li>
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span> this can be the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form#attr-id">id</a></code>
 attribute of any <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form">&lt;form&gt;</a></code>
 element in the same document. Even if the attribute is set on <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input">&lt;input&gt;</a></code>
, this property will be <code>null</code>, if it isn't the id of a <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form">&lt;form&gt;</a></code>
 element.</li> <li>
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span> this must be <code>null</code>.</li> </ul> */
	var form(default,null) : FormElement;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-formaction">formaction</a></code>
 HTML attribute, containing the URI&nbsp;of a program that processes information submitted by the element. If specified, this attribute overrides the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form#attr-action">action</a></code>
 attribute of the <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form">&lt;form&gt;</a></code>
 element that owns this element. */
	var formAction : String;

	var formEnctype : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-formmethod">formmethod</a></code>
&nbsp;HTML&nbsp;attribute, containing the HTTP&nbsp;method that the browser uses to submit the form. If specified, this attribute overrides the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form#attr-method">method</a></code>
 attribute of the <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form">&lt;form&gt;</a></code>
 element that owns this element. */
	var formMethod : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-formnovalidate">formnovalidate</a></code>
&nbsp;HTML&nbsp;attribute, indicating that the form is not to be validated when it is submitted. If specified, this attribute overrides the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form#attr-novalidate">novalidate</a></code>
 attribute of the <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form">&lt;form&gt;</a></code>
 element that owns this element. */
	var formNoValidate : Bool;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-formtarget">formtarget</a></code>
 HTML&nbsp;attribute, containing a name or keyword indicating where to display the response that is received after submitting the form. If specified, this attribute overrides the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form#attr-target">target</a></code>
 attribute of the <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form">&lt;form&gt;</a></code>
 element that owns this element. */
	var formTarget : String;

	var grammar : Bool;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-height">height</a></code>
 HTML attribute, which defines the height of the image displayed for the button, if the value of <strong>type</strong> is <span>image</span>. */
	var height : Int;

	var incremental : Bool;

	/** Indicates that a checkbox is neither on nor off. */
	var indeterminate : Bool;

	/** A list of <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/label">&lt;label&gt;</a></code>
 elements that are labels for this element. */
	var labels(default,null) : NodeList;

	/** Identifies a list of pre-defined options to suggest to the user. The value must be the <strong>id</strong> of a <code><a class="new" href="https://developer.mozilla.org/en/HTML/Element/datalist" rel="internal">&lt;datalist&gt;</a></code> element in the same document. The browser displays only options that are valid values for this input element. This attribute is ignored when the <strong>type</strong> attribute's value is <span>hidden</span>, <span>checkbox</span>, <span>radio</span>, <span>file</span>, or a button type. */
	var list(default,null) : Element;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-max">max</a></code>
 HTML&nbsp;attribute, containing the maximum (numeric or date-time) value for this item, which must not be less than its minimum (<strong>min</strong> attribute) value. */
	var max : String;

	/** <p>Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-maxlength">maxlength</a></code>
&nbsp;HTML attribute, containing the maximum length of text (in Unicode code points) that the value can be changed to. The constraint is evaluated only when the value is changed</p> <div class="note"><strong>Note:</strong> If you set <code>maxlength</code> to a negative value programmatically, an exception will be thrown.</div> Setter throws DOMException. */
	var maxLength : Int;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-min">min</a></code>
 HTML&nbsp;attribute, containing the minimum (numeric or date-time) value for this item, which must not be greater than its maximum (<strong>max</strong> attribute) value. */
	var min : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-multiple">multiple</a></code>
 HTML&nbsp;attribute, indicating whether more than one value is possible (e.g., multiple files). */
	var multiple : Bool;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-name">name</a></code>
 HTML&nbsp;attribute, containing a name that identifies the element when submitting the form. */
	var name : String;

	var onspeechchange : EventListener;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-pattern">pattern</a></code>
 HTML&nbsp;attribute, containing a regular expression that the control's value is checked against. The pattern must match the entire value, not just some subset. Use the <strong>title</strong> attribute to describe the pattern to help the user. This attribute applies when the value of the <strong>type</strong> attribute is <span>text</span>, <span>search</span>, <span>tel</span>, <span>url</span> or <span>email</span>; otherwise it is ignored. */
	var pattern : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-placeholder">placeholder</a></code>
 HTML&nbsp;attribute, containing a hint to the user of what can be entered in the control. The placeholder text must not contain carriage returns or line-feeds. This attribute applies when the value of the <strong>type</strong> attribute is <span>text</span>, <span>search</span>, <span>tel</span>, <span>url</span> or <span>email</span>; otherwise it is ignored. */
	var placeholder : String;

	/** <p>Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-readonly">readonly</a></code>
 HTML&nbsp;attribute, indicating that the user cannot modify the value of the control.</p> <p><span><a href="https://developer.mozilla.org/en/HTML/HTML5" rel="custom nofollow">HTML 5</a></span> This is ignored if the value of the <strong>type</strong> attribute is <span>hidden</span>, <span>range</span>, <span>color</span>, <span>checkbox</span>, <span>radio</span>, <span>file</span>, or a button type.</p> */
	var readOnly : Bool;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-required">required</a></code>
 HTML&nbsp;attribute, indicating that the user must fill in a value before submitting a form. */
	var required : Bool;

	/** The direction in which selection occurred. This is "forward" if selection was performed in the start-to-end direction of the current locale, or "backward" for the opposite direction. This can also be "none"&nbsp;if the direction is unknown." */
	var selectionDirection : String;

	/** The index of the end of selected text. */
	var selectionEnd : Int;

	/** The index of the beginning of selected text. */
	var selectionStart : Int;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-size">size</a></code>
 HTML&nbsp;attribute, containing size of the control. This value is in pixels unless the value of <strong>type</strong> is <span>text</span> or <span>password</span>, in which case, it is an integer number of characters. 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span> Applies only when <strong>type</strong> is set to <span>text</span>, <span>search</span>, <span>tel</span>, <span>url</span>, <span>email</span>, or <span>password</span>; otherwise it is ignored. Setter throws DOMException. */
	var size : Int;

	var speech : Bool;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-src">src</a></code>
 HTML&nbsp;attribute, which specifies a URI for the location of an image to display on the graphical submit button, if the value of <strong>type</strong> is <span>image</span>; otherwise it is ignored. */
	var src : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-step">step</a></code>
 HTML&nbsp;attribute, which works with<strong> min</strong> and <strong>max</strong> to limit the increments at which a numeric or date-time value can be set. It can be the string <span>any</span> or a positive floating point number. If this is not set to <span>any</span>, the control accepts only values at multiples of the step value greater than the minimum. */
	var step : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-type">type</a></code>
 HTML&nbsp;attribute, indicating the type of control to display. See 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-type">type</a></code>
 attribute of <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input">&lt;input&gt;</a></code>
 for possible values. */
	var type : String;

	/** A client-side image map. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span> */
	var useMap : String;

	/** A localized message that describes the validation constraints that the control does not satisfy (if any). This is the empty string if the control is not a candidate for constraint validation (<strong>willValidate</strong> is false), or it satisfies its constraints. */
	var validationMessage(default,null) : String;

	/** The validity states that this element is in.&nbsp; */
	var validity(default,null) : ValidityState;

	/** Current value in the control. Setter throws DOMException. */
	var value : String;

	/** The value of the element, interpreted as a date, or <code>null</code> if conversion is not possible. Setter throws DOMException. */
	var valueAsDate : Date;

	/** <p>The value of the element, interpreted as one of the following in order:</p> <ol> <li>a time value</li> <li>a number</li> <li><code>null</code> if conversion is not possible</li> </ol> Setter throws DOMException. */
	var valueAsNumber : Float;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input#attr-width">width</a></code>
 HTML&nbsp;attribute, which defines the width of the image displayed for the button, if the value of <strong>type</strong> is <span>image</span>. */
	var width : Int;

	/** Indicates whether the element is a candidate for constraint validation. It is false if any conditions bar it from constraint validation. */
	var willValidate(default,null) : Bool;

	function checkValidity() : Bool;

	function select() : Void;

	function setCustomValidity( error : String ) : Void;

	/** Throws DOMException. */
	@:overload( function( replacement : String ) :Void {} )
	function setRangeText( replacement : String, start : Int, end : Int, selectionMode : String ) : Void;

	function setSelectionRange( start : Int, end : Int, ?direction : String ) : Void;

	function stepDown( ?n : Int ) : Void;

	function stepUp( ?n : Int ) : Void;

}
