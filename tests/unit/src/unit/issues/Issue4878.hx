package unit.issues;
#if java
import java.vm.Thread;
import java.vm.Mutex;
#end

class Issue4878 extends Test {
  function test() {
#if java
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
#end
  }
}
