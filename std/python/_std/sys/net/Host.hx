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
package sys.net;

/**
	A given IP host name.
**/
class Host {

	/**
		The actual IP corresponding to the host.
	**/
	public var ip(default,null) : Int;

	/**
		Creates a new Host : the name can be an IP in the form "127.0.0.1" or an host name such as "google.com", in which case
		the corresponding IP address is resolved using DNS. An exception occur if the host name could not be found.
	**/
    var name:String;
	public function new( name : String ) : Void {
        this.name = name;
    }

	/**
		Returns the IP representation of the host
	**/
	public function toString() : String {
        return name;
    }

	/**
		Perform a reverse-DNS query to resolve a host name from an IP.
	**/
	public function reverse() : String {
        return "";
    }

	/**
		Returns the local computer host name
	**/
	public static function localhost() : String {
        return "";
    }

}