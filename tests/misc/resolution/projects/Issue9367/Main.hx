import utest.Assert.equals;

class Main extends utest.Test {
	function test() {
		equals("subtype1subtype2static3", pack.UsageNoImport.f());
		equals("pack.Mod3.lowerCase", pack.UsageNoImport.f2());
		equals("field1subtype2field3", pack.UsageImport.f());
		equals("pack.Mod3.lowerCase", pack.UsageImport.f2());
	}

	static function main() {
		utest.UTest.run([new Main()]);
	}
}
