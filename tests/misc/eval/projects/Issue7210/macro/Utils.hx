package macro;

class Utils {
	macro static function hello() {
		Sys.stderr().writeString('Ok!');
		return macro {}
	}
}