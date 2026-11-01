class Main {
	static function main() {
		var foo = '5';
		var bar = '5';
		var isEqual = switch (bar) {
			case 'foo$ foo': true;
			case '\\ foo': true;
			case '$foo': true;
			default: false;
		}
	}
}
