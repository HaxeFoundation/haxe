package unit;
#if neko
import neko.vm.Thread;
import neko.vm.Deque;
import neko.vm.Lock;
import neko.vm.Tls;
import neko.vm.Mutex;
#elseif cpp
import cpp.vm.Thread;
import cpp.vm.Deque;
import cpp.vm.Lock;
import cpp.vm.Tls;
import cpp.vm.Mutex;
#elseif java
import java.vm.Thread;
import java.vm.Deque;
import java.vm.Lock;
import java.vm.Tls;
import java.vm.Mutex;
#end

class TestThreads extends Test
{

	private function testSort()
	{
		var ts = new ThreadSort();
#if java
		ts.maxVal *= 10;
#end
		for (creatorWait in [.2, .5, 0])
			for (creatorLoad in [false, true])
				for (consumerWait in [.2,.5,0])
					for (useTls in [false,true])
						for (q in [new QDeque() #if java, new QLockFree()#end])
							for (lock1 in [new DequeSemaphore(), new LockSemaphore()])
								for (lock2 in [new DequeSemaphore(), new LockSemaphore()])
								{
									ts.creatorWait = creatorWait;
									ts.creatorLoad = creatorLoad;
									ts.consumerWait = consumerWait;
									ts.useTls = useTls;
									ts.queue = q;
									ts.lock1 = lock1;
									ts.lock2 = lock2;
									try
									{
										ts.run();

										trace('Passed: $creatorWait, $creatorLoad, $consumerWait, $useTls, $q, $lock1, $lock2');
										t(true);
									}
									catch(e:Dynamic)
									{
										Test.report('Error $e for parameters: $creatorWait, $creatorLoad, $consumerWait, $useTls, $q, $lock1, $lock2');
									}
								}
	}
}

class ThreadSort
{
	//params
	public var creatorWait:Seconds = .2;
	//should an extra load be performed on the creator?
	public var creatorLoad:Bool = false;
	public var consumerWait:Seconds = 0;
	//should an extra load be performed on the consumer?
	public var consumerLoad:Bool = false;
	public var useTls:Bool = false;

	public var nThreads:Int = 10;
	public var maxVal:Int = 100000;

	public var queue:QueueStrategy<Int>;
	public var lock1:SemaphoreStrategy;
	public var lock2:SemaphoreStrategy;

	public function new()
	{

	}

	public function run()
	{
		//spawning creators
		lock1.setReleaseCount(nThreads);
		lock2.setReleaseCount(nThreads);
		var finishedMutex = new Mutex();
		finishedMutex.acquire();

		var tls = new Tls();
		for (i in 0...nThreads)
		{
			Thread.create(function() {
				tls.value = i;
				Sys.sleep(creatorWait.secs());
				var i = useTls ? tls.value : i;
				for (j in 1...maxVal)
				{
					if (j % nThreads == i)
					{
						queue.add(j);
					}
					if (j % 10000 == 0 && creatorLoad)
						Sys.sleep(.1);
				}
				//creator thread finished
				lock1.release();
			});
		}

		//spawning consumers
		if (consumerWait != 0)
			Sys.sleep(consumerWait.secs());

		var arr = [];
		for (i in 0...nThreads)
		{
			var myarr = [];
			arr.push(myarr);
			Thread.create(function() {
				var i = 0;
				while(true)
				{
					var val = queue.pop();
					if (val != null) {
						myarr.push(val);
					} else if (finishedMutex.tryAcquire()) {
						finishedMutex.release();
						break;
					}
					if (++i % 10000 == 0 && consumerLoad)
						Sys.sleep(.1);
				}
				lock2.release();
			});
		}

		//wait creators to finish
		lock1.block();

		//release finishedMutex
		finishedMutex.release();

		//wait consumers to finish
		lock2.block();

		//start checking
		var mainarr = [];
		mainarr[maxVal] = maxVal; //prealloc

		for(a in arr)
		{
			for (a in a)
			{
				var val = mainarr[a];
				if (val == a)
					throw 'Same value detected for $val and $a';
				mainarr[a] = a;
			}
		}

		//no repeats, ok!

		for (i in 1...mainarr.length)
		{
			if (i != mainarr[i])
				throw 'No value found for $i and ${mainarr[i]}';
		}
	}
}

private interface SemaphoreStrategy //may be released by another thread
{
	function block():Void; //block until all semaphores are released
	function release():Void;
	function setReleaseCount(i:Int):Void;
}

private class DequeSemaphore implements SemaphoreStrategy
{
	var d:Deque<Bool>;
	var c:Int = 0;

	public function new()
	{
		this.d = new Deque();
		this.c = 0;
	}

	public function block()
	{
		for (i in 0...c)
		{
			d.pop(true);
		}
		this.c = 0;
	}

	public function release()
	{
		d.push(true);
	}

	public function setReleaseCount(c:Int)
	{
		this.c = c;
	}
}

private class LockSemaphore implements SemaphoreStrategy
{
	var l:Lock;
	var c:Int = 0;

	public function new()
	{
		this.l = new Lock();
		this.l.release();
	}

	public function block()
	{
		for(i in 0...c)
		{
			l.wait();
		}
		this.c = 0;
	}

	public function release()
	{
		this.l.release();
	}

	public function setReleaseCount(c:Int)
	{
		this.c = c;
	}
}


private interface QueueStrategy<T>
{
	function add(t:T):Void;
	function pop():Null<T>; //not blocking
}

private class QDeque<T> implements QueueStrategy<T>
{
	var q:Deque<T>;
	public function new()
	{
		this.q = new Deque();
	}

	public function add(t:T)
	{
		this.q.add(t);
	}

	public function pop():Null<T>
	{
		return this.q.pop(false);
	}
}

#if java
private class QLockFree<T> implements QueueStrategy<T>
{
	var q:java.vm.AtomicList<T>;

	public function new()
	{
		this.q = new java.vm.AtomicList();
	}

	public function add(t:T)
	{
		this.q.add(t);
	}

	public function pop():Null<T>
	{
		return this.q.pop();
	}
}
#end

private abstract Seconds(Float) from Float
{
	public inline function secs():Float
	{
		return this;
	}

	public inline function ms():Float
	{
		return this * 1000;
	}
}
