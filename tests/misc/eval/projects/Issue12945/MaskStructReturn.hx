typedef R = { x:Int, y:Int };

function take(?n:Int, ?f:Void->R) {}

function main() {
	take(() -> { x: 1 });
}
