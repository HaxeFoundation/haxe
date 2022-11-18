class ImmediatelyInvoked {
	static function main() {
        var test:String = function(str) { return str; }("hello");
    }
}
