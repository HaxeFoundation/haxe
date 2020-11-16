package cases;

import hl.uv.Timer;
import haxe.Timer as HxTimer;

class TestTimer extends TestBase {
	static inline function sameInterval(expected:Float, actual:Float, ?pos:haxe.PosInfos) {
		floatEquals(expected, actual, expected * 0.05, pos);
	}

	function testInitStartStop() {
		var timer = Timer.init(loop);
		var cnt = 0;
		var tm = HxTimer.stamp();
		timer.start(() -> {
			++cnt;
			var dt = (HxTimer.stamp() - tm) * 1000;
			if(cnt == 1) {
				sameInterval(50, dt);
				tm += 0.05;
			} else if(cnt <= 3) {
				sameInterval(100, dt);
				tm += 0.1;
			} else {
				timer.stop();
				timer.close();
			}
		}, 50, 100);
		loop.run(Default);
	}

	@:depends(testInitStartStop)
	function testAgain() {
		var timer = Timer.init(loop);
		var cnt = 0;
		var tm = HxTimer.stamp();
		timer.start(() -> {
			++cnt;
			var dt = (HxTimer.stamp() - tm) * 1000;
			if(cnt <= 2) {
				sameInterval(50, dt);
				tm += 0.05;
			} else {
				timer.stop();
				timer.close();
			}
		}, 100, 50);
		timer.again();
		loop.run(Default);
	}

	@:depends(testInitStartStop)
	function testRepeat_getSet() {
		var timer = Timer.init(loop);
		var cnt = 0;
		var tm = HxTimer.stamp();
		timer.start(() -> {
			++cnt;
			var dt = (HxTimer.stamp() - tm) * 1000;
			if(cnt == 1) {
				var repeat = timer.repeat;
				equals(50, repeat);
				timer.repeat = repeat * 2;
			} else if(cnt <= 2) {
				sameInterval(50, dt);
				tm += 0.1;
			} else {
				timer.stop();
				timer.close();
			}
			tm += 0.05;
		}, 0, 50);
		timer.again();
		loop.run(Default);
	}
}