package unit.issues;
import haxe.Int64;
using haxe.Int64;

class Issue2752 extends Test {
	function test() {
		var big : Int64 = Int64.make(2000,1000);
		var call = function(){
			eq(big.getHigh(),2000);
			eq(big.getLow(),1000);
		}
		call();
	}
}

