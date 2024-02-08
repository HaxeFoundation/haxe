class Main {
	static function main() {
		function loop(type) {
			type.args.field;
			loop(type.args);
		}
	}
}