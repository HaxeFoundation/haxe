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
package sys.io;

/**
Allows system process creation similar to Sys.command but with more finer control.

Warning : 
Whence the 'process' is created, you have to call exitCode() and retrieve it to terminate the call sequence.
Warning :
Whence the 'process' is created, it can starve if stderr and stdout are not emptied regularly. 
System buffers can be pretty small so you might encounter starvation quick.
Solution is either to work with programs that have very small sterr/stdout footprints, either launch threads to manage 
the system channels and de-starve 'process'.
Use with caution and prefer Sys.command if need be.

@example
var p = new sys.io.Process('ls','-lsa');
p.exitCode();

*/
extern class Process {

	var stdout(default,null) : haxe.io.Input;
	var stderr(default,null) : haxe.io.Input;
	var stdin(default,null) : haxe.io.Output;

	function new( cmd : String, args : Array<String> ) : Void;
	function getPid() : Int;
	
	/**
	Retrieve process exit codes allowing its termination
	*/
	function exitCode() : Int; 
	function close() : Void;
	function kill() : Void;

}
