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

private typedef ThreadHandle = hl.Abstract<"hl_thread">;

abstract ThreadImpl(ThreadHandle) {

	@:hlNative("std", "thread_create")
	public static function create(callb:Void->Void):ThreadImpl {
		return null;
	}

	@:hlNative("std", "thread_current")
	public static function current():ThreadImpl {
		return null;
	}

	public static function setName( impl : ThreadImpl, name : String ) {
		#if (hl_ver >= version("1.13.0"))
		set_name(impl, @:privateAccess name.toUtf8());
		#end
	}

	public static function getName( impl : ThreadImpl ) : Null<String> {
		#if (hl_ver >= version("1.13.0"))
		var name = get_name(impl);
		return name == null ? null : @:privateAccess String.fromUTF8(name);
		#else
		return null;
		#end
	}

	#if (hl_ver >= version("1.13.0"))
	@:hlNative("?std", "thread_set_name") static function set_name( t : ThreadImpl, name : hl.Bytes ) {}
	@:hlNative("?std", "thread_get_name") static function get_name( t : ThreadImpl ) : hl.Bytes { return null; }
	#end

}
