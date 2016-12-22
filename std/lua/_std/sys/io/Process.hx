/*
 * Copyright (C)2005-2016 Haxe Foundation
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
import lua.Io;
import lua.FileHandle;
import lua.Boot;
import lua.Io;

@:coreApi
class Process {
	var fh : FileHandle;
	var eh : FileHandle;
	var errTmpFile : String;

	public var stdout(default,null) : haxe.io.Input;
	public var stderr(default,null) : haxe.io.Input;
	public var stdin(default, null) : haxe.io.Output;
	public function new( cmd : String, ?args : Array<String>){
		if (args == null) args = [];
		this.errTmpFile = Boot.tempFile();
		this.eh = Io.open(errTmpFile, 'r');
		args.push('>2 ${Boot.tempFile()}');
		cmd = Boot.shellEscapeCmd(cmd, args);
		this.fh = Io.popen(cmd, "w+");
		this.stdout = new FileInput(fh);
		this.stderr = new FileInput(eh);
		this.stdin = new FileOutput(fh);
	}
	public function getPid() : Int {
		return 0;
	}

	public function close() : Void { }

	public function exitCode( block : Bool = true ) : Null<Int> { return 0; }

	public function kill() : Void { }
}
