package haxe.coro.schedulers;

import haxe.exceptions.ArgumentException;

class VirtualTimeScheduler extends EventLoopScheduler {
	var currentTime : Int64;

	public function new() {
		super();

		currentTime = 0i64;
	}

	public override function now() {
		return currentTime;
	}

	public function advanceBy(ms:Int) {
		if (ms < 0) {
			throw new ArgumentException("Time must be greater or equal to zero");
		}

		virtualRun(currentTime + ms);
	}

	public function advanceTo(ms:Int) {
		if (ms < 0) {
			throw new ArgumentException("Time must be greater or equal to zero");
		}
		if (ms < currentTime) {
			throw new ArgumentException("Cannot travel back in time");
		}

		virtualRun(ms);
	}

	function virtualRun(endTime : Int64) {
		while (true) {
			for (event in zeroEvents.flip()) {
				event();
			}

			while (true) {
				if (first == null) {
					last = null;
					break;
				}
				if (first.runTime <= endTime) {
					final toRun = first;
					currentTime = first.runTime;
					first = first.next;
					if (first != null) {
						first.previous = null;
					}
					toRun.run();
				} else {
					break;
				}
			}
			if (zeroEvents.empty()) {
				break;
			}
		}

		currentTime = endTime;
	}
}