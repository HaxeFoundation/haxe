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

/** A <code>DeviceOrientationEvent</code> object describes an event that provides information about the current orientation of the device as compared to the Earth coordinate frame. See <a title="Orientation and motion data explained" rel="internal" href="https://developer.mozilla.org/en/DOM/Orientation_and_motion_data_explained">Orientation and motion data explained</a> for details.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/DeviceOrientationEvent">MDN</a>. */
@:native("DeviceOrientationEvent")
extern class DeviceOrientationEvent extends Event
{
	/** This attribute's value is <code>true</code> if the orientation is provided as a difference between the device coordinate frame and the Earth coordinate frame; if the device can't detect the Earth coordinate frame, this value is <code>false</code>. <strong>Read only.</strong> */
	var absolute(default,null) : Bool;

	/** The current orientation of the device around the Z axis; that is, how far the device is rotated around a line perpendicular to the device. <strong>Read only.</strong> */
	var alpha(default,null) : Float;

	/** The current orientation of the device around the X axis; that is, how far the device is tipped forward or backward. <strong>Read only.</strong> */
	var beta(default,null) : Float;

	/** <dl><dd>The current orientation of the device around the Y axis; that is, how far the device is turned left or right. <strong>Read only.</strong></dd>
</dl>
<div class="note"><strong>Note:</strong> If the browser is not able to provide notification information, all values are 0.</div> */
	var gamma(default,null) : Float;

	function initDeviceOrientationEvent( type : String, bubbles : Bool, cancelable : Bool, alpha : Float, beta : Float, gamma : Float, absolute : Bool ) : Void;

}
