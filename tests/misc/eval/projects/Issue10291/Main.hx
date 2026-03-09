import haxe.ds.Option;

class Main {
	static function main() {
		var o = Some({b: null});
		switch (o) {
			case Some({b: b}) if (b != null):
				trace("Case 1: b=" + b);
			case Some({b: null}):
				trace("Case 2: b=null");
			case None: // Change None to _ and it works
				trace("Default");
		}
	}
}