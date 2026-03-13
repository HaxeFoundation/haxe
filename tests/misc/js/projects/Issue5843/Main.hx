package;

import Data;

class Main {
	static function main() {
		trace('main');
		var item:Item = null;
		trace(item);
	}
}

typedef Item = {
	?data:Transformed, // remove the `?` to make it compile
}
