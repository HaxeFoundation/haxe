package cases.issues;

// Compiler warnings should be routed via write_out (the \x01-prefixed prints
// channel) rather than write_err (raw bytes) in server mode.  In --server-connect
// mode, write_err writes raw bytes into the same buffer as write_result (the JSON
// response), so warning text in write_err can corrupt result.stderr and break
// JSON-RPC parsing on the client side.
class Issue12830 extends TestCase {
	function testWarningsDoNotPolluteStederr(_) {
		vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
		// -D eval_times is deprecated in favor of -D times.eval and triggers a
		// WDeprecatedDefine warning that exercises the non-error message routing path.
		var args = ["-main", "HelloWorld.hx", "--no-output", "-js", "no.js", "-D", "eval_times"];
		runHaxe(args);
		assertSuccess();
		// Warning must appear in prints (via write_out / \x01 channel), not stderr.
		Assert.isTrue(lastResult.prints.exists(p -> p.contains("WDeprecatedDefine")));
		Assert.isFalse(lastResult.stderr.contains("WDeprecatedDefine"));
	}
}
