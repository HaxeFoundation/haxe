package java.vm;
import java.Lib;

/**
	A Lock-free Queue implementation
**/
@:native('haxe.java.vm.Deque')
@:nativeGen class Deque<T>
{
	@:private var head:Node<T>;
	@:private var tail:Node<T>;

	public function new()
	{
		this.head = this.tail = new Node(null);
	}

	public function add(i : T)
	{
		var n = new Node(i);
		untyped __lock__(this,
		{
			tail.next = n;
			tail = n;
			try { untyped this.notify(); } catch(e:Dynamic) { throw e; }
		});
	}

	public function push(i : T)
	{
		var n = new Node(i);
		untyped __lock__(this,
		{
			n.next = head.next;
			head.next = n;
			try { untyped this.notify(); } catch(e:Dynamic) { throw e; }
		});
	}

	public function pop(block : Bool) : Null<T>
	{
		var ret = null;
		untyped __lock__(this, {
			var n = null;
			do {
				n = head.next;
				if (n != null)
				{
					ret = n.value;
					n.value = null;
					head = n;
				} else if (block) {
					//block
					try { untyped this.wait(); } catch(e:Dynamic) { throw e; }
				}
			} while( block && n == null );
		});
		return ret;
	}
}

@:native('haxe.java.vm.DequeNode')
@:nativeGen
class Node<T>
{
	public var value:T;
	public var next:Node<T>;

	public function new(val)
	{
		this.value = val;
	}
}
