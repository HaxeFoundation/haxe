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
package sys.io;

import python.io.IoTools;
import python.lib.io.BufferedReader;
import python.lib.io.BufferedWriter;
import python.lib.io.TextIOWrapper;
import python.lib.Subprocess;
import python.lib.subprocess.Popen;

class Process {

	public var stdout(default,null) : haxe.io.Input;
	public var stderr(default,null) : haxe.io.Input;
	public var stdin(default,null) : haxe.io.Output;

	var p:Popen;

	public function new( cmd : String, ?args : Array<String> ) : Void {
		p = Popen.create(args == null ? cmd : [cmd].concat(args), { shell : args == null, stdin : Subprocess.PIPE, stdout: Subprocess.PIPE, stderr : Subprocess.PIPE });
		this.stdout = IoTools.createFileInputFromText(new TextIOWrapper(new BufferedReader(p.stdout)));
		this.stderr = IoTools.createFileInputFromText(new TextIOWrapper(new BufferedReader(p.stderr)));
		this.stdin =  IoTools.createFileOutputFromText(new TextIOWrapper(new BufferedWriter(p.stdin)));
	}

	public function getPid() : Int {
		return p.pid;
	}
	public function exitCode( block : Bool = true ) : Null<Int> {
		if( block == false )
			return p.poll();
		return p.wait();
	}
	public function close() : Void {
		var ver = python.lib.Sys.version_info;
		if (ver[0] > 3 || (ver[0] == 3 && ver[1] >= 3)) // >= 3.3
			try {
				p.terminate();
			} catch (e:python.Exceptions.ProcessLookupError) {
				// it has already terminated 
			}
		else
			try {
				p.terminate();
			} catch (e:python.Exceptions.OSError) {
				// it has already terminated 
			}
	}
	public function kill() : Void {
		p.kill();
	}

}