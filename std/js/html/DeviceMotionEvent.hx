/*
 * Copyright (C)2005-2012 Haxe Foundation
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

/** A <code>DeviceMotionEvent</code> object describes an event that indicates the amount of physical motion of the device that has occurred, and is fired at a set interval (rather than in response to motion). It provides information about the rate of rotation, as well as acceleration along all three axes.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/DeviceMotionEvent">MDN</a>. */
@:native("DeviceMotionEvent")
extern class DeviceMotionEvent extends Event
{
    /** The acceleration of the device. This value has taken into account the effect of gravity and removed it from the figures. This value may not exist if the hardware doesn't know how to remove gravity from the acceleration data. <strong>Read only.</strong> */
    var acceleration (default,null) :DeviceAcceleration;

    /** The acceleration of the device. This value includes the effect of gravity, and may be the only value available on devices that don't have a gyroscope to allow them to properly remove gravity from the data. <strong>Read only.</strong> */
    var accelerationIncludingGravity (default,null) :DeviceAcceleration;

    /** The interval, in milliseconds, at which the <code>DeviceMotionEvent</code> is fired. The next event will be fired in approximately this amount of time. */
    var interval (default,null) :Float;

    /** The rates of rotation of the device about all three axes. <strong>Read only.</strong> */
    var rotationRate (default,null) :DeviceRotationRate;

    function initDeviceMotionEvent (type :String, bubbles :Bool, cancelable :Bool, acceleration :DeviceAcceleration, accelerationIncludingGravity :DeviceAcceleration, rotationRate :DeviceRotationRate, interval :Float) :Void;

}
