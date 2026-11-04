typedef Intersection = VueConstructor & {};

typedef VueConstructor = {
	@:overload(function(?options:Intersection):Void { })
	function extend():Void;
};

class Main {
    static function main() {}
}