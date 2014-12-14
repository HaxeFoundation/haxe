package unit.issues;

private class Node<T>
{
    public var cur:T;
    public var next:Null<Node<T>>;
    public function new(cur,?next)
    {
        this.cur = cur;
        this.next = next;
    }
}

class Issue3681 extends Test {
	function test() {
        var i:Null<Int> = null;
        var nodes = new Node(i,new Node(1,new Node(2, new Node(i))));
        var matches = switch (nodes)
        {
            case { cur : null, next : ({ cur : 1, next : _ }) }:
                true;
            case _:
                false;
        }
        t(matches);
	}
}