// This is in fact DCE related, so it should be compiled seperately...
class Main {
	static function main() {
		var i = 2;
		i >>>= 1;
		if (i != 1)
			throw "i != 1";

		if (StringTools.hex(1, 6) != "000001")
			throw 'StringTools.hex(1, 6) != "000001"';
	}
}