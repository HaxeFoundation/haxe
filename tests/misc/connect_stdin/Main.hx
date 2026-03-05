import sys.io.Process;

using StringTools;

class Main {
	static function main() {
		var port = 7300;

		// Start the compilation server
		var server = new Process("haxe", ["--wait", Std.string(port)]);
		Sys.sleep(1.0);

		var failures = 0;
		var count = 0;

		function test(name:String, f:() -> Bool) {
			count++;
			Sys.print('  Test: $name ... ');
			if (f()) {
				Sys.println("OK");
			} else {
				Sys.println("FAILED");
				failures++;
			}
		}

		// Test 1: Piped stdin through --connect
		test("stdin line forwarding", () -> {
			var client = new Process("haxe", ["--connect", Std.string(port), "-cp", ".", "--main", "StdinEcho", "--interp"]);
			client.stdin.writeString("hello world\n");
			client.stdin.close();
			var stdout = client.stdout.readAll().toString().trim();
			var exitCode = client.exitCode();
			client.close();
			if (stdout != "Got: hello world") {
				Sys.println('\n    Expected: "Got: hello world"');
				Sys.println('    Got: "$stdout"');
				return false;
			}
			return exitCode == 0;
		});

		// Test 2: Multiple lines through --connect
		test("stdin multiline forwarding", () -> {
			var client = new Process("haxe", [
				"--connect", Std.string(port), "-cp", ".", "--main", "StdinMultiline", "--interp"
			]);
			client.stdin.writeString("line1\nline2\nline3\n");
			client.stdin.close();
			var stdout = client.stdout.readAll().toString().trim();
			var exitCode = client.exitCode();
			client.close();
			if (stdout != "Got 3 lines: line1, line2, line3") {
				Sys.println('\n    Expected: "Got 3 lines: line1, line2, line3"');
				Sys.println('    Got: "$stdout"');
				return false;
			}
			return exitCode == 0;
		});

		// Test 3: Piped stdin through --cmd
		test("stdin line forwarding to command", () -> {
			var client = new Process("haxe", ["--connect", Std.string(port), "--cmd", "cat -"]);
			client.stdin.writeString("hello world\n");
			client.stdin.close();
			var stdout = client.stdout.readAll().toString().trim();
			var exitCode = client.exitCode();
			client.close();
			if (stdout != "hello world") {
				Sys.println('\n    Expected: "hello world"');
				Sys.println('    Got: "$stdout"');
				return false;
			}
			return exitCode == 0;
		});

		// Test 4: No stdin consumed (just compile) - should not hang
		test("no-stdin request completes", () -> {
			var client = new Process("haxe", ["--connect", Std.string(port), "-cp", ".", "--main", "StdinEcho", "--no-output"]);
			client.stdin.close();
			var exitCode = client.exitCode();
			client.close();
			return exitCode == 0;
		});

		// Clean up the server
		server.kill();
		server.close();

		Sys.println('Done running $count tests with $failures failures');
		Sys.exit(failures);
	}
}
