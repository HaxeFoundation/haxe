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

package cpp.uv;

import cpp.uv.Stream;

using cpp.uv.UV;

enum abstract TtyMode(Int) {
	var MODE_NORMAL;
	var MODE_RAW;
	var MODE_IO;

	@:to inline function toUv():UvTtyModeT
		return cast this;
}

enum abstract VTermState(Int) {
	var SUPPORTED;
	var UNSUPPORTED;

	@:to inline function toUv():UvTtyVtermstateT
		return cast this;

	@:from static inline function fromUv(state:UvTtyVtermstateT):VTermState
		return cast state;
}

/**
	TTY handles represent a stream for the console.

	@see http://docs.libuv.org/en/v1.x/tty.html
**/
@:headerCode('#include "uv.h"')
class Tty extends Stream {
	var uvTty(get,never):RawPointer<UvTtyT>;

	inline function get_uvTty():RawPointer<UvTtyT>
		return cast uv;

	override function setupUvData() {
		uv = cast UvTtyT.create();
		super.setupUvData();
	}

	/**
		Initialize a new TTY stream with the given file descriptor.
	**/
	static public function init(loop:Loop, file:File):Tty {
		var tty = new Tty(loop);
		UV.tty_init(loop.uvLoop, tty.uvTty, file.uvFile, 0).resolve();
		tty.referenceFromLoop();
		return tty;
	}

	/**
		Set the TTY using the specified terminal mode.
	**/
	public function setMode(mode:TtyMode) {
		UV.tty_set_mode(uvTty, mode).resolve();
	}

	/**
		To be called when the program exits.
		Resets TTY settings to default values for the next process to take over.
	**/
	static public function resetMode() {
		UV.tty_reset_mode().resolve();
	}

	/**
		Gets the current Window size.
	**/
	public function getWinSize():{width:Int, height:Int} {
		var width = 0;
		var height = 0;
		UV.tty_get_winsize(uvTty, RawPointer.addressOf(width), RawPointer.addressOf(height)).resolve();
		return {width:width, height:height};
	}

	/**
		Controls whether console virtual terminal sequences are processed by libuv
		or console.

		This function is only meaningful on Windows systems. On Unix it is silently
		ignored.
	**/
	static public function setVTermState(state:VTermState) {
		UV.tty_set_vterm_state(state);
	}

	/**
		Get the current state of whether console virtual terminal sequences are
		handled by libuv or the console.

		This function is not implemented on Unix, where it throws `UV_ENOTSUP`.
	**/
	static public function getVTermState(state:VTermState):VTermState {
		var state = UV_TTY_UNSUPPORTED;
		UV.tty_get_vterm_state(RawPointer.addressOf(state)).resolve();
		return state;
	}

}