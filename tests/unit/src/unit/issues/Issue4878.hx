package unit.issues;
#if java
import sys.thread.Thread;
import sys.thread.Mutex;
#end

class Issue4878 extends Test {
	#if java
  function test() {
    var mutex = new Mutex();
    var thread = Thread.create(function() {
      mutex.acquire();
      mutex.acquire();
      mutex.release();
      Sys.sleep(.2);
      mutex.release();
    });
    Sys.sleep(0.05);
    f(mutex.tryAcquire());
    Sys.sleep(.3);
    t(mutex.tryAcquire());
  }
  #end
}
