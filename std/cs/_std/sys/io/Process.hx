/*
 * Copyright (C)2005-2019 Haxe Foundation
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

import cs.system.diagnostics.Process as NativeProcess;
import cs.system.diagnostics.ProcessStartInfo as NativeStartInfo;

@:coreApi
class Process {
	public var stdout(default, null):haxe.io.Input;
	public var stderr(default, null):haxe.io.Input;
	public var stdin(default, null):haxe.io.Output;

	private var native:NativeProcess;

	public function new(cmd:String, ?args:Array<String>, ?detached:Bool):Void {
		if (detached)
			throw "Detached process is not supported on this platform";
		this.native = createNativeProcess(cmd, args);
		native.Start();

		this.stdout = new cs.io.NativeInput(native.StandardOutput.BaseStream);
		this.stderr = new cs.io.NativeInput(native.StandardError.BaseStream);
		this.stdin = new cs.io.NativeOutput(native.StandardInput.BaseStream);
	}

	@:allow(Sys)
	private static function createNativeProcess(cmd:String, ?args:Array<String>):NativeProcess {
		var native = new NativeProcess();
		native.StartInfo.CreateNoWindow = true;
		native.StartInfo.RedirectStandardError = native.StartInfo.RedirectStandardInput = native.StartInfo.RedirectStandardOutput = true;
		native.StartInfo.UseShellExecute = false;
		// Check if we need to unset any environment variables
		// In .NET AOT, ProcessStartInfo.Environment modifications don't work reliably,
		// so we use `env -u VAR` to unset variables before running the command
		var removedVars = Sys.getRemovedEnvVars();
		// Check if there are actually any removed vars
		var hasRemovedVars = false;
		if (removedVars != null) {
			for (_ in removedVars.keys()) {
				hasRemovedVars = true;
				break;
			}
		}
		var needsEnvWrapper = hasRemovedVars && Sys.systemName() != "Windows";
		if (needsEnvWrapper && args != null) {
			// Use /usr/bin/env -u VAR to unset environment variables
			// This avoids shell escaping issues by passing arguments directly via ArgumentList
			native.StartInfo.FileName = "/usr/bin/env";
			var startInfo = native.StartInfo;
			// Add -u for each removed var
			for (key in removedVars.keys()) {
				cs.Syntax.code("{0}.ArgumentList.Add({1})", startInfo, "-u");
				cs.Syntax.code("{0}.ArgumentList.Add({1})", startInfo, key);
			}
			// Add the command - resolve path if needed
			if (cmd.indexOf("/") != -1 || cmd.indexOf("\\") != -1)
				cmd = sys.FileSystem.fullPath(cmd);
			cs.Syntax.code("{0}.ArgumentList.Add({1})", startInfo, cmd);
			// Add all arguments
			for (arg in args) {
				cs.Syntax.code("{0}.ArgumentList.Add({1})", startInfo, arg);
			}
		} else if (args != null) {
			// Use ArgumentList collection (like JVM/Python) - no escaping needed
			if (cmd.indexOf("/") != -1 || cmd.indexOf("\\") != -1)
				cmd = sys.FileSystem.fullPath(cmd);
			native.StartInfo.FileName = cmd;
			var startInfo = native.StartInfo;
			for (arg in args) {
				cs.Syntax.code("{0}.ArgumentList.Add({1})", startInfo, arg);
			}
		} else {
			// Shell command - use /bin/sh -c (or cmd.exe /C on Windows)
			switch (Sys.systemName()) {
				case "Windows":
					native.StartInfo.FileName = switch (Sys.getEnv("COMSPEC")) {
						case null: "cmd.exe";
						case comspec: comspec;
					}
					native.StartInfo.Arguments = '/C "$cmd"';
				case _:
					native.StartInfo.FileName = "/bin/sh";
					var startInfo = native.StartInfo;
					cs.Syntax.code("{0}.ArgumentList.Add({1})", startInfo, "-c");
					cs.Syntax.code("{0}.ArgumentList.Add({1})", startInfo, cmd);
			}
		}
		return native;
	}

	public function getPid():Int {
		return native.Id;
	}

	public function exitCode(block:Bool = true):Null<Int> {
		if (block == false && !native.HasExited)
			return null;
		native.WaitForExit();
		return native.ExitCode;
	}

	public function close():Void {
		native.Close();
	}

	public function kill():Void {
		native.Kill();
	}
}
