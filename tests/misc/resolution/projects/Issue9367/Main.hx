import utest.Assert.equals;

class Main extends utest.Test {
	function test() {
		equals("subtype1subtype2static3", pack.UsageNoImport.f());
		equals("field1subtype2field3", pack.UsageImport.f());
	}

	static function main() {
		utest.UTest.run([new Main()]);
	}
}
