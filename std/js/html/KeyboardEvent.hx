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

// This file is generated from mozilla\KeyboardEvent.webidl. Do not edit!

package js.html;

/**
	`KeyboardEvent` objects describe a user interaction with the keyboard. Each event describes a key; the event type (`keydown`, `keypress`, or `keyup`) identifies what kind of activity was performed.

	Documentation [KeyboardEvent](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent>
**/
@:native("KeyboardEvent")
extern class KeyboardEvent extends UIEvent
{
	static inline var DOM_KEY_LOCATION_STANDARD : Int = 0;
	static inline var DOM_KEY_LOCATION_LEFT : Int = 1;
	static inline var DOM_KEY_LOCATION_RIGHT : Int = 2;
	static inline var DOM_KEY_LOCATION_NUMPAD : Int = 3;
	static inline var DOM_VK_CANCEL : Int = 3;
	static inline var DOM_VK_HELP : Int = 6;
	static inline var DOM_VK_BACK_SPACE : Int = 8;
	static inline var DOM_VK_TAB : Int = 9;
	static inline var DOM_VK_CLEAR : Int = 12;
	static inline var DOM_VK_RETURN : Int = 13;
	static inline var DOM_VK_SHIFT : Int = 16;
	static inline var DOM_VK_CONTROL : Int = 17;
	static inline var DOM_VK_ALT : Int = 18;
	static inline var DOM_VK_PAUSE : Int = 19;
	static inline var DOM_VK_CAPS_LOCK : Int = 20;
	static inline var DOM_VK_KANA : Int = 21;
	static inline var DOM_VK_HANGUL : Int = 21;
	static inline var DOM_VK_EISU : Int = 22;
	static inline var DOM_VK_JUNJA : Int = 23;
	static inline var DOM_VK_FINAL : Int = 24;
	static inline var DOM_VK_HANJA : Int = 25;
	static inline var DOM_VK_KANJI : Int = 25;
	static inline var DOM_VK_ESCAPE : Int = 27;
	static inline var DOM_VK_CONVERT : Int = 28;
	static inline var DOM_VK_NONCONVERT : Int = 29;
	static inline var DOM_VK_ACCEPT : Int = 30;
	static inline var DOM_VK_MODECHANGE : Int = 31;
	static inline var DOM_VK_SPACE : Int = 32;
	static inline var DOM_VK_PAGE_UP : Int = 33;
	static inline var DOM_VK_PAGE_DOWN : Int = 34;
	static inline var DOM_VK_END : Int = 35;
	static inline var DOM_VK_HOME : Int = 36;
	static inline var DOM_VK_LEFT : Int = 37;
	static inline var DOM_VK_UP : Int = 38;
	static inline var DOM_VK_RIGHT : Int = 39;
	static inline var DOM_VK_DOWN : Int = 40;
	static inline var DOM_VK_SELECT : Int = 41;
	static inline var DOM_VK_PRINT : Int = 42;
	static inline var DOM_VK_EXECUTE : Int = 43;
	static inline var DOM_VK_PRINTSCREEN : Int = 44;
	static inline var DOM_VK_INSERT : Int = 45;
	static inline var DOM_VK_DELETE : Int = 46;
	static inline var DOM_VK_0 : Int = 48;
	static inline var DOM_VK_1 : Int = 49;
	static inline var DOM_VK_2 : Int = 50;
	static inline var DOM_VK_3 : Int = 51;
	static inline var DOM_VK_4 : Int = 52;
	static inline var DOM_VK_5 : Int = 53;
	static inline var DOM_VK_6 : Int = 54;
	static inline var DOM_VK_7 : Int = 55;
	static inline var DOM_VK_8 : Int = 56;
	static inline var DOM_VK_9 : Int = 57;
	static inline var DOM_VK_COLON : Int = 58;
	static inline var DOM_VK_SEMICOLON : Int = 59;
	static inline var DOM_VK_LESS_THAN : Int = 60;
	static inline var DOM_VK_EQUALS : Int = 61;
	static inline var DOM_VK_GREATER_THAN : Int = 62;
	static inline var DOM_VK_QUESTION_MARK : Int = 63;
	static inline var DOM_VK_AT : Int = 64;
	static inline var DOM_VK_A : Int = 65;
	static inline var DOM_VK_B : Int = 66;
	static inline var DOM_VK_C : Int = 67;
	static inline var DOM_VK_D : Int = 68;
	static inline var DOM_VK_E : Int = 69;
	static inline var DOM_VK_F : Int = 70;
	static inline var DOM_VK_G : Int = 71;
	static inline var DOM_VK_H : Int = 72;
	static inline var DOM_VK_I : Int = 73;
	static inline var DOM_VK_J : Int = 74;
	static inline var DOM_VK_K : Int = 75;
	static inline var DOM_VK_L : Int = 76;
	static inline var DOM_VK_M : Int = 77;
	static inline var DOM_VK_N : Int = 78;
	static inline var DOM_VK_O : Int = 79;
	static inline var DOM_VK_P : Int = 80;
	static inline var DOM_VK_Q : Int = 81;
	static inline var DOM_VK_R : Int = 82;
	static inline var DOM_VK_S : Int = 83;
	static inline var DOM_VK_T : Int = 84;
	static inline var DOM_VK_U : Int = 85;
	static inline var DOM_VK_V : Int = 86;
	static inline var DOM_VK_W : Int = 87;
	static inline var DOM_VK_X : Int = 88;
	static inline var DOM_VK_Y : Int = 89;
	static inline var DOM_VK_Z : Int = 90;
	static inline var DOM_VK_WIN : Int = 91;
	static inline var DOM_VK_CONTEXT_MENU : Int = 93;
	static inline var DOM_VK_SLEEP : Int = 95;
	static inline var DOM_VK_NUMPAD0 : Int = 96;
	static inline var DOM_VK_NUMPAD1 : Int = 97;
	static inline var DOM_VK_NUMPAD2 : Int = 98;
	static inline var DOM_VK_NUMPAD3 : Int = 99;
	static inline var DOM_VK_NUMPAD4 : Int = 100;
	static inline var DOM_VK_NUMPAD5 : Int = 101;
	static inline var DOM_VK_NUMPAD6 : Int = 102;
	static inline var DOM_VK_NUMPAD7 : Int = 103;
	static inline var DOM_VK_NUMPAD8 : Int = 104;
	static inline var DOM_VK_NUMPAD9 : Int = 105;
	static inline var DOM_VK_MULTIPLY : Int = 106;
	static inline var DOM_VK_ADD : Int = 107;
	static inline var DOM_VK_SEPARATOR : Int = 108;
	static inline var DOM_VK_SUBTRACT : Int = 109;
	static inline var DOM_VK_DECIMAL : Int = 110;
	static inline var DOM_VK_DIVIDE : Int = 111;
	static inline var DOM_VK_F1 : Int = 112;
	static inline var DOM_VK_F2 : Int = 113;
	static inline var DOM_VK_F3 : Int = 114;
	static inline var DOM_VK_F4 : Int = 115;
	static inline var DOM_VK_F5 : Int = 116;
	static inline var DOM_VK_F6 : Int = 117;
	static inline var DOM_VK_F7 : Int = 118;
	static inline var DOM_VK_F8 : Int = 119;
	static inline var DOM_VK_F9 : Int = 120;
	static inline var DOM_VK_F10 : Int = 121;
	static inline var DOM_VK_F11 : Int = 122;
	static inline var DOM_VK_F12 : Int = 123;
	static inline var DOM_VK_F13 : Int = 124;
	static inline var DOM_VK_F14 : Int = 125;
	static inline var DOM_VK_F15 : Int = 126;
	static inline var DOM_VK_F16 : Int = 127;
	static inline var DOM_VK_F17 : Int = 128;
	static inline var DOM_VK_F18 : Int = 129;
	static inline var DOM_VK_F19 : Int = 130;
	static inline var DOM_VK_F20 : Int = 131;
	static inline var DOM_VK_F21 : Int = 132;
	static inline var DOM_VK_F22 : Int = 133;
	static inline var DOM_VK_F23 : Int = 134;
	static inline var DOM_VK_F24 : Int = 135;
	static inline var DOM_VK_NUM_LOCK : Int = 144;
	static inline var DOM_VK_SCROLL_LOCK : Int = 145;
	static inline var DOM_VK_WIN_OEM_FJ_JISHO : Int = 146;
	static inline var DOM_VK_WIN_OEM_FJ_MASSHOU : Int = 147;
	static inline var DOM_VK_WIN_OEM_FJ_TOUROKU : Int = 148;
	static inline var DOM_VK_WIN_OEM_FJ_LOYA : Int = 149;
	static inline var DOM_VK_WIN_OEM_FJ_ROYA : Int = 150;
	static inline var DOM_VK_CIRCUMFLEX : Int = 160;
	static inline var DOM_VK_EXCLAMATION : Int = 161;
	static inline var DOM_VK_DOUBLE_QUOTE : Int = 162;
	static inline var DOM_VK_HASH : Int = 163;
	static inline var DOM_VK_DOLLAR : Int = 164;
	static inline var DOM_VK_PERCENT : Int = 165;
	static inline var DOM_VK_AMPERSAND : Int = 166;
	static inline var DOM_VK_UNDERSCORE : Int = 167;
	static inline var DOM_VK_OPEN_PAREN : Int = 168;
	static inline var DOM_VK_CLOSE_PAREN : Int = 169;
	static inline var DOM_VK_ASTERISK : Int = 170;
	static inline var DOM_VK_PLUS : Int = 171;
	static inline var DOM_VK_PIPE : Int = 172;
	static inline var DOM_VK_HYPHEN_MINUS : Int = 173;
	static inline var DOM_VK_OPEN_CURLY_BRACKET : Int = 174;
	static inline var DOM_VK_CLOSE_CURLY_BRACKET : Int = 175;
	static inline var DOM_VK_TILDE : Int = 176;
	static inline var DOM_VK_VOLUME_MUTE : Int = 181;
	static inline var DOM_VK_VOLUME_DOWN : Int = 182;
	static inline var DOM_VK_VOLUME_UP : Int = 183;
	static inline var DOM_VK_COMMA : Int = 188;
	static inline var DOM_VK_PERIOD : Int = 190;
	static inline var DOM_VK_SLASH : Int = 191;
	static inline var DOM_VK_BACK_QUOTE : Int = 192;
	static inline var DOM_VK_OPEN_BRACKET : Int = 219;
	static inline var DOM_VK_BACK_SLASH : Int = 220;
	static inline var DOM_VK_CLOSE_BRACKET : Int = 221;
	static inline var DOM_VK_QUOTE : Int = 222;
	static inline var DOM_VK_META : Int = 224;
	static inline var DOM_VK_ALTGR : Int = 225;
	static inline var DOM_VK_WIN_ICO_HELP : Int = 227;
	static inline var DOM_VK_WIN_ICO_00 : Int = 228;
	static inline var DOM_VK_WIN_ICO_CLEAR : Int = 230;
	static inline var DOM_VK_WIN_OEM_RESET : Int = 233;
	static inline var DOM_VK_WIN_OEM_JUMP : Int = 234;
	static inline var DOM_VK_WIN_OEM_PA1 : Int = 235;
	static inline var DOM_VK_WIN_OEM_PA2 : Int = 236;
	static inline var DOM_VK_WIN_OEM_PA3 : Int = 237;
	static inline var DOM_VK_WIN_OEM_WSCTRL : Int = 238;
	static inline var DOM_VK_WIN_OEM_CUSEL : Int = 239;
	static inline var DOM_VK_WIN_OEM_ATTN : Int = 240;
	static inline var DOM_VK_WIN_OEM_FINISH : Int = 241;
	static inline var DOM_VK_WIN_OEM_COPY : Int = 242;
	static inline var DOM_VK_WIN_OEM_AUTO : Int = 243;
	static inline var DOM_VK_WIN_OEM_ENLW : Int = 244;
	static inline var DOM_VK_WIN_OEM_BACKTAB : Int = 245;
	static inline var DOM_VK_ATTN : Int = 246;
	static inline var DOM_VK_CRSEL : Int = 247;
	static inline var DOM_VK_EXSEL : Int = 248;
	static inline var DOM_VK_EREOF : Int = 249;
	static inline var DOM_VK_PLAY : Int = 250;
	static inline var DOM_VK_ZOOM : Int = 251;
	static inline var DOM_VK_PA1 : Int = 253;
	static inline var DOM_VK_WIN_OEM_CLEAR : Int = 254;
	
	
	/**
		Returns a `Number` representing the Unicode reference number of the key; this attribute is used only by the `keypress` event. For keys whose `char` attribute contains multiple characters, this is the Unicode value of the first character in that attribute. In Firefox 26 this returns codes for printable characters.
		 Warning: This attribute is deprecated; you should use `KeyboardEvent.key` instead, if available.
		 
	**/
	var charCode(default,null) : Int;
	
	/**
		Returns a `Number` representing a system and implementation dependent numerical code identifying the unmodified value of the pressed key.
		 Warning: This attribute is deprecated; you should use `KeyboardEvent.key` instead, if available.
		 
	**/
	var keyCode(default,null) : Int;
	
	/**
		Returns a `Boolean` that is `true` if the Alt ( Option or ⌥ on OS X) key was active when the key event was generated.
	**/
	var altKey(default,null) : Bool;
	
	/**
		Returns a `Boolean` that is `true` if the Ctrl key was active when the key event was generated.
	**/
	var ctrlKey(default,null) : Bool;
	
	/**
		Returns a `Boolean` that is `true` if the Shift key was active when the key event was generated.
	**/
	var shiftKey(default,null) : Bool;
	
	/**
		Returns a `Boolean` that is `true` if the Meta key (on Mac keyboards, the ⌘ Command key; on Windows keyboards, the Windows key (⊞)) was active when the key event was generated.
	**/
	var metaKey(default,null) : Bool;
	
	/**
		Returns a `Number` representing the location of the key on the keyboard or other input device.
	**/
	var location(default,null) : Int;
	
	/**
		Returns a `Boolean` that is `true` if the key is being held down such that it is automatically repeating.
	**/
	var repeat(default,null) : Bool;
	
	/**
		Returns a `Boolean` that is `true` if the event is fired between after `compositionstart` and before `compositionend`.
	**/
	var isComposing(default,null) : Bool;
	
	/**
		Returns a `DOMString` representing the key value of the key represented by the event.
	**/
	var key(default,null) : String;
	
	/** @throws DOMError */
	function new( typeArg : String, ?keyboardEventInitDict : KeyboardEventInit ) : Void;
	
	/**
		Returns a `Boolean` indicating if the modifier key, like Alt, Shift, Ctrl, or Meta, was pressed when the event was created.
	**/
	function getModifierState( key : String ) : Bool;
	
	/**
		Initializes a `KeyboardEvent` object. This has only been implemented by Gecko (others used `KeyboardEvent.initKeyboardEvent()`) and should not be used any more. The standard modern way is to use the `KeyboardEvent.KeyboardEvent` constructor.
	**/
	function initKeyEvent( type : String, canBubble : Bool, cancelable : Bool, view : Window, ctrlKey : Bool, altKey : Bool, shiftKey : Bool, metaKey : Bool, keyCode : Int, charCode : Int ) : Void;
}