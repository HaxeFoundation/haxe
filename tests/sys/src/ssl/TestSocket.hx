package ssl;

import sys.ssl.*;
import utest.Assert;

class TestSocket extends utest.Test {
	function testImmediateClose() {
		try {
			final socket = new Socket();
			socket.close();
			Assert.pass();
		} catch (e) {
			Assert.fail('Closing unused socket failed: ${e}');
		}
	}
}
