package cases.issues;

// Tests that Sys.stdout() and Sys.stderr() route output through the server
// communication protocol when running --interp via the compilation server.
class Issue9359 extends TestCase {
	// Sys.stdout().writeString() reaches the client via prints (0x01-prefixed protocol),
	// Sys.stderr().writeString() reaches the client via stderr channel.
	function testStdoutStderrRouting(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue9359/Main.hx"));
		var args = ["-main", "Main", "--interp"];
		runHaxe(args);
		assertSuccess();
		assertHasPrint("stdout line");
		Assert.isTrue(lastResult.stderr.contains("stderr line"));
	}

	// Sys.print() and Sys.println() already use com.print, so they route correctly.
	// This ensures they remain unaffected by the stdout/stderr fix.
	function testSysPrint(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue9359/SysPrint.hx"));
		var args = ["-main", "Main", "--interp"];
		runHaxe(args);
		assertSuccess();
		assertHasPrint("sys_print_output");
	}

	// Sys.command() in server mode routes stdout through write_out (prints channel)
	// and stderr through write_err (stderr channel). This covers #10742.
	function testSysCommand(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue9359/SysCommand.hx"));
		var args = ["-main", "Main", "--interp"];
		runHaxe(args);
		assertSuccess();
		assertHasPrint("command_out");
	}

	// --cmd in server mode routes the command's stdout/stderr through the protocol.
	// This covers #10742 and #11424.
	function testCmdRouting(_) {
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		runHaxe(["-main", "Empty", "--interp", "--cmd", "echo cmd_out"]);
		assertSuccess();
		assertHasPrint("cmd_out");
	}
}
