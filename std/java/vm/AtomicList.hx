package java.vm;

import java.util.concurrent.atomic.AtomicReference;

/**
	A lock-free queue implementation
**/
@:native('haxe.java.vm.AtomicList')
@:nativeGen class AtomicList<T>
{
	@:volatile @:private var head:AtomicNode<T>;
	@:volatile @:private var tail:AtomicReference<AtomicNode<T>>;

	public function new()
	{
		this.head = new AtomicNode(null);
		this.head.set(new AtomicNode(null));
		this.tail = new AtomicReference(head);
	}

	public function add(v:T)
	{
		var n = new AtomicNode(v), tail = this.tail;
		var p = null;
		while( !((p = tail.get()).compareAndSet(null, n)) )
		{
			tail.compareAndSet(p, p.get());
		}
		tail.compareAndSet(p, n);
	}

	public function pop():Null<T>
	{
		var p = null, pget = null, head = head;
		do
		{
			p = head.get();
			if ( (pget = p.get()) == null)
				return null; //empty
		} while(!head.compareAndSet(p, pget));

		var ret = pget.value;
		pget.value = null;
		return ret;
	}

	public function peek()
	{
		var ret = head.get();
		if (ret == null) return null; //empty
		return ret.value;
	}

	public function peekLast()
	{
		return tail.get().value;
	}

}

@:native('haxe.java.vm.AtomicNode')
@:nativeGen class AtomicNode<T> extends AtomicReference<AtomicNode<T>>
{
	public var value:T;

	public function new(value)
	{
		super();
		this.value = value;
	}

}
