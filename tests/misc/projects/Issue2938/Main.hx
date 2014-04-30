import haxe.ds.Vector;

class Main {
    public static function main() {
		var width = 9;
		var slots = new Vector(width);
		for(i in 0...width){
			slots[i] = i;
		}
		trace(Std.string(slots));
    }
}