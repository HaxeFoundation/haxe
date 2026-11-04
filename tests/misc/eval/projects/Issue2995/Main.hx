class Main {
    function new() {}

    static function main() {
        var m = new Main();
    }

	static function something() {
        new Main();
	}
}