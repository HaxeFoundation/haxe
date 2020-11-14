package cases;

import sys.thread.IThreadPool;
import sys.thread.FixedThreadPool;

class TestFixedThreadPool extends misc.TestThreadPoolBase {
	override function createThreadPool(count:Int):IThreadPool {
		return new FixedThreadPool(count);
	}
}