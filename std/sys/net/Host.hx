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
package sys.net;

/**
	A given IP host name.
**/
extern class Host {

	/**
		The provided host string.
	**/
	var host(default,null) : String;

	/**
		The actual IP corresponding to the host.
	**/
	var ip(default,null) : Int;

	/**
		Creates a new Host : the name can be an IP in the form "127.0.0.1" or an host name such as "google.com", in which case
		the corresponding IP address is resolved using DNS. An exception occur if the host name could not be found.
	**/
	function new( name : String ) : Void;

	/**
		Returns the IP representation of the host
	**/
	function toString() : String;

	/**
		Perform a reverse-DNS query to resolve a host name from an IP.
	**/
	function reverse() : String;

	/**
		Returns the local computer host name
	**/
	static function localhost() : String;

}
