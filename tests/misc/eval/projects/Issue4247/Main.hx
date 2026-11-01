class Main1 {
	static function main() {
		var x:Dynamic;
		if (Math.random() < 0.5) x=3;
		else x = [];
		switch(x) {
			case 3 : trace(x);
			case [] : trace(x);
		}
	}
}