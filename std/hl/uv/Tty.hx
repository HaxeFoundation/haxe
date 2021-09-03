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

package hl.uv;

using hl.uv.UV;

enum abstract TtyMode(Int) to Int {
	/** Initial/normal terminal mode */
	var TTY_MODE_NORMAL;
	/** Raw input mode (On Windows, ENABLE_WINDOW_INPUT is also enabled) */
	var TTY_MODE_RAW;
	/** Binary-safe I/O mode for IPC (Unix-only) */
	var TTY_MODE_IO;
}

/**
	Console virtual terminal mode type
**/
enum abstract TtyVTermState(Int) to Int {
	/**
		The console supports handling of virtual terminal sequences (Windows10
		new console, ConEmu)
	**/
	var TTY_SUPPORTED = 0;
	/** The console cannot process virtual terminal sequences. (Legacy console) */
	var TTY_UNSUPPORTED = 1;
}

/**
	TTY handles represent a stream for the console

	@see http://docs.libuv.org/en/v1.x/tty.html
**/
class Tty extends Stream<UvTtyTStar> {
	/**
		Initialize a new TTY stream with the given file descriptor.
	**/
	static public function init(loop:Loop, fd:File):Tty {
		loop.checkLoop();
		var tty = new Tty(UV.alloc_tty());
		var result = loop.tty_init(tty.h, fd, 0);
		if(result < 0) {
			tty.freeHandle();
			result.throwErr();
		}
		return tty;
	}

	/**
		Set the TTY using the specified terminal mode.
	**/
	public function setMode(mode:TtyMode):Void {
		handle(h -> h.tty_set_mode(mode).resolve());
	}

	/**
		To be called when the program exits. Resets TTY settings to default values
		for the next process to take over.
	**/
	static public function resetMode():Void {
		UV.tty_reset_mode().resolve();
	}

	/**
		Gets the current Window size.
	**/
	public function getWinSize():{width:Int, height:Int} {
		return handleReturn(h -> {
			var width = 0;
			var height = 0;
			h.tty_get_winsize(Ref.make(width), Ref.make(height)).resolve();
			return {width:width, height:height}
		});
	}

	/**
		Controls whether console virtual terminal sequences are processed by libuv
		or console.

		Useful in particular for enabling ConEmu support of ANSI X3.64 and Xterm 256
		colors. Otherwise Windows10 consoles are usually detected automatically.

		This function is only meaningful on Windows systems. On Unix it is silently
		ignored.
	**/
	static public function setVTermState(state:TtyVTermState):Void {
		UV.tty_set_vterm_state(state);
	}

	/**
		Get the current state of whether console virtual terminal sequences are
		handled by libuv or the console.

		This function is not implemented on Unix, where it throws `ENOTSUP`.
	**/
	static public function getVTermState():TtyVTermState {
		var state = TTY_UNSUPPORTED;
		UV.tty_get_vterm_state(Ref.make(state)).resolve();
		return state;
	}
}
