package unit;

import utest.Assert;
import haxe.PosInfos;

class TestOperators extends Test {
	@:analyzer(ignore)
	function specBitwise_overflow32() {
		var zero = 0;
		var allOnes = -1;
		bin('1111 1111    1111 1111    1111 1111    1111 1111') == ~zero;
		bin('1111 1111    1111 1111    1111 1111    1111 1110') == allOnes << 1;
		bin('1111 1111    1111 1111    1111 1111    1111 1111') == allOnes >> 1;
		bin('0111 1111    1111 1111    1111 1111    1111 1111') == allOnes >>> 1;
		bin('1111 1111    1111 1111    1111 1111    1111 1111') == allOnes | 0;
		bin('1111 1111    1111 1111    1111 1111    1111 1111') == allOnes & allOnes;
		bin('0000 0000    0000 0000    0000 0000    0000 0000') == allOnes ^ thirtyTwoOnes();
	}

	function thirtyTwoOnes() {
		return bin('1111 1111 1111 1111 1111 1111 1111 1111');
	}

	function bin(binary:String, ?pos:PosInfos):Int {
		var result = 0;
		var bitPos:Int = 32;

		for(c in binary) {
			switch(c) {
				case ' '.code | '\t'.code:
				case '0'.code:
					bitPos--;
				case '1'.code:
					bitPos--;
					result = result | 1 << bitPos;
				case _:
					Assert.fail('Invalid character "${String.fromCharCode(c)}"', pos);
			}
		}
		if(bitPos != 0) {
			Assert.fail('Binary value should contain exactly 32 bits', pos);
		}

		return result;
	}
}