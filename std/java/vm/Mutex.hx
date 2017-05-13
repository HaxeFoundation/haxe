/*
 * Copyright (C)2005-2017 Haxe Foundation
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
 package java.vm;
import java.util.concurrent.locks.ReentrantLock;

@:native('haxe.java.vm.Mutex') class Mutex
{
	@:private var lock:ReentrantLock;

	/**
		Creates a mutex, which can be used to acquire a temporary lock to access some resource.
		The main difference with a lock is that a mutex must always be released by the owner thread
	**/
	public function new()
	{
		this.lock = new ReentrantLock();
	}

	/**
		Try to acquire the mutex, returns true if acquire or false if it's already locked by another thread.
	**/
	public function tryAcquire():Bool
	{
		return this.lock.tryLock();
	}

	/**
		The current thread acquire the mutex or wait if not available.
		The same thread can acquire several times the same mutex, but must release it as many times it has been acquired.
	**/
	public function acquire():Void
	{
		this.lock.lock();
	}

	/**
		Release a mutex that has been acquired by the current thread. If the current thread does not own the mutex, an exception will be thrown
	**/
	public function release():Void
	{
		this.lock.unlock();
	}
}
