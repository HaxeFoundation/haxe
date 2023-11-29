import haxe.ds.Option;

class Main {
	public static function main() {
		switch ((null:Option<Int>)) {
			case Some(value):
			case None:
		}
	}
}
