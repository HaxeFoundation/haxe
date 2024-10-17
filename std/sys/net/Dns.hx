/*
 * Copyright (C)2005-2024 Haxe Foundation
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
	Provides domain name resolution tools.
**/
extern class Dns {
	/**
		Tries to resolve the IP addresses for the given hostname.

		This call is blocking.
		This operation may or may not retrieve from any caches, including the operating system one.

		@param name The host name to resolve.
		@return An unsorted array of IP addresses. Can be empty if the host could not be resolved.
	**/
	static function resolveSync(name:String):Array<IpAddress>;

	/**
		Tries to run a reverse DNS lookup on the given IP address.

		This call is blocking.
		This operation may or may not retrieve from any caches, including the operating system one.

		@param address The IPv4 or IPv6 address to reverse lookup.
		@return An unsorted array of hostnames pointing to the given address.
		Can be empty if the address could not be resolved.
	**/
	static function reverseSync(address:IpAddress):Array<String>;

	/**
		Returns the hostname (computer name, etc.) of the machine running the current process.
	**/
	static function getLocalHostname():String;
}
