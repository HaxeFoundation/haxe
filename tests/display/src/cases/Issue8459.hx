package cases;

class Issue8459 extends DisplayTestCase {
	/**
		class Some {
			function main() {
				$type({-1-}
			}
		}
	**/
	function testDollarType() {
		sigEq(0, [["expression:Unknown<0>"]], signature(pos(1)));
	}
}
