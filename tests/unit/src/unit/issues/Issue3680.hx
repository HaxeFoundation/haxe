package unit.issues;

private class Node<T> {
    public var cur:T;
    public var next:Null<Node<T>>;
    public function new(cur)
    {
        this.cur = cur;
    }
}

class Issue3680 extends Test {
	function test() {
		var a = "";
        switch (new Node(1))
        {
            case { cur: 1, next: ({ cur : 2 }) }:
                a = "assert";
            default:
                a = 'correct';
        }
		eq("correct", a);
	}
}