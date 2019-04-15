/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package sys.thread;
import java.Lib;
import java.lang.System;
using haxe.Int64;

@:coreApi
@:native('haxe.java.vm.Lock') class Lock
{
	@:private @:volatile var releasedCount = 0;

	public function new()
	{
	}

	public function wait(?timeout : Float) : Bool
	{
		var ret = false;
		java.Lib.lock(this,
		{
			if (--releasedCount < 0)
			{
				if (timeout == null)
				{
					// since .notify() is asynchronous, this `while` is needed
					// because there is a very remote possibility of release() awaking a thread,
					// but before it releases, another thread calls wait - and since the release count
					// is still positive, it will get the lock.
					while( releasedCount < 0 )
					{
						try
						{
							(cast this : java.lang.Object).wait();
						}
						catch(e:java.lang.InterruptedException)
						{
						}
					}
				} else {
					var timeout:haxe.Int64 = cast timeout * 1000;
					var cur = System.currentTimeMillis(),
					    max = cur.add(timeout);
					// see above comment about this while loop
					while ( releasedCount < 0 && cur.compare(max) < 0 )
					{
						try
						{
							var t = max.sub(cur);
							(cast this : java.lang.Object).wait(t);
							cur = System.currentTimeMillis();
						}
						catch(e:java.lang.InterruptedException)
						{
						}
					}
				}
			}
			ret = this.releasedCount >= 0;
			if (!ret)
				this.releasedCount++; //timed out
		});
		return ret;
	}

	public function release():Void
	{
		untyped __lock__(this,
		{
			if (++releasedCount >= 0)
			{
				untyped this.notify();
			}
		});
	}
}
