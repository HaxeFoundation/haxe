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
package js.html.svg;

/** <p>The <code>SVGLength</code> interface correspond to the <a title="https://developer.mozilla.org/en/SVG/Content_type#Length" rel="internal" href="https://developer.mozilla.org/en/SVG/Content_type#Length">&lt;length&gt;</a> basic data type.</p>
<p>An <code>SVGLength</code> object can be designated as read only, which means that attempts to modify the object will result in an exception being thrown.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/Document_Object_Model_(DOM)/SVGLength">MDN</a>. */
@:native("SVGLength")
extern class Length
{
	/** A value was specified using the cm units defined in CSS2. */
	static inline var SVG_LENGTHTYPE_CM : Int = 6;

	/** A value was specified using the em units defined in CSS2. */
	static inline var SVG_LENGTHTYPE_EMS : Int = 3;

	/** A value was specified using the ex units defined in CSS2. */
	static inline var SVG_LENGTHTYPE_EXS : Int = 4;

	/** A value was specified using the in units defined in CSS2. */
	static inline var SVG_LENGTHTYPE_IN : Int = 8;

	/** A value was specified using the mm units defined in CSS2. */
	static inline var SVG_LENGTHTYPE_MM : Int = 7;

	/** No unit type was provided (i.e., a unitless value was specified), which indicates a value in user units. */
	static inline var SVG_LENGTHTYPE_NUMBER : Int = 1;

	/** A value was specified using the pc units defined in CSS2. */
	static inline var SVG_LENGTHTYPE_PC : Int = 10;

	/** A percentage value was specified. */
	static inline var SVG_LENGTHTYPE_PERCENTAGE : Int = 2;

	/** A value was specified using the pt units defined in CSS2. */
	static inline var SVG_LENGTHTYPE_PT : Int = 9;

	/** A value was specified using the px units defined in CSS2. */
	static inline var SVG_LENGTHTYPE_PX : Int = 5;

	/** The unit type is not one of predefined unit types. It is invalid to attempt to define a new value of this type or to attempt to switch an existing value to this type. */
	static inline var SVG_LENGTHTYPE_UNKNOWN : Int = 0;

	/** The type of the value as specified by one of the SVG_LENGTHTYPE_* constants defined on this interface. */
	var unitType (default,null) : Int;

	/** <p>The value as a floating point value, in user units. Setting this attribute will cause <code>valueInSpecifiedUnits</code> and <code>valueAsString</code> to be updated automatically to reflect this setting.</p> <p><strong>Exceptions on setting:</strong> a <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/DOMException">DOMException</a></code>
 with code <code>NO_MODIFICATION_ALLOWED_ERR</code> is raised when the length corresponds to a read only attribute or when the object itself is read only.</p> Getter throws DOMException. Setter throws DOMException. */
	var value : Float;

	/** <p>The value as a string value, in the units expressed by <code>unitType</code>. Setting this attribute will cause <code>value</code>, <code>valueInSpecifiedUnits</code> and <code>unitType</code> to be updated automatically to reflect this setting.</p> <p><strong>Exceptions on setting:</strong></p> <ul> <li>a <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/DOMException">DOMException</a></code>
 with code <code>SYNTAX_ERR</code> is raised if the assigned string cannot be parsed as a valid <a title="https://developer.mozilla.org/en/SVG/Content_type#Length" rel="internal" href="https://developer.mozilla.org/en/SVG/Content_type#Length">&lt;length&gt;</a>.</li> <li>a <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/DOMException">DOMException</a></code>
 with code <code>NO_MODIFICATION_ALLOWED_ERR</code> is raised when the length corresponds to a read only attribute or when the object itself is read only.</li> </ul> Setter throws DOMException. */
	var valueAsString : String;

	/** <p>The value as a floating point value, in the units expressed by <code>unitType</code>. Setting this attribute will cause <code>value</code> and <code>valueAsString</code> to be updated automatically to reflect this setting.</p> <p><strong>Exceptions on setting:</strong> a <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/DOMException">DOMException</a></code>
 with code <code>NO_MODIFICATION_ALLOWED_ERR</code> is raised when the length corresponds to a read only attribute or when the object itself is read only.</p> */
	var valueInSpecifiedUnits : Float;

	function convertToSpecifiedUnits( unitType : Int ) : Void;

	function newValueSpecifiedUnits( unitType : Int, valueInSpecifiedUnits : Float ) : Void;

}
