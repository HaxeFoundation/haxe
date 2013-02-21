package java.io;
/*
* Copyright (c) 1996, 2005, Oracle and/or its affiliates. All rights reserved.
* DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
*
* This code is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License version 2 only, as
* published by the Free Software Foundation.  Oracle designates this
* particular file as subject to the "Classpath" exception as provided
* by Oracle in the LICENSE file that accompanied this code.
*
* This code is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
* version 2 for more details (a copy is included in the LICENSE file that
* accompanied this code).
*
* You should have received a copy of the GNU General Public License version
* 2 along with this work; if not, write to the Free Software Foundation,
* Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
*
* Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
* or visit www.oracle.com if you need additional information or have any
* questions.
*/
/**
* Abstract class for writing to character streams.  The only methods that a
* subclass must implement are write(char[], int, int), flush(), and close().
* Most subclasses, however, will override some of the methods defined here in
* order to provide higher efficiency, additional functionality, or both.
*
* @see Writer
* @see   BufferedWriter
* @see   CharArrayWriter
* @see   FilterWriter
* @see   OutputStreamWriter
* @see     FileWriter
* @see   PipedWriter
* @see   PrintWriter
* @see   StringWriter
* @see Reader
*
* @author      Mark Reinhold
* @since       JDK1.1
*/
@:require(java1) extern class Writer implements java.lang.Appendable implements java.io.Closeable implements java.io.Flushable
{
	/**
	* The object used to synchronize operations on this stream.  For
	* efficiency, a character-stream object may use an object other than
	* itself to protect critical sections.  A subclass should therefore use
	* the object in this field rather than <tt>this</tt> or a synchronized
	* method.
	*/
	private var lock : Dynamic;
	
	/**
	* Creates a new character-stream writer whose critical sections will
	* synchronize on the writer itself.
	*/
	@:overload private function new() : Void;
	
	/**
	* Creates a new character-stream writer whose critical sections will
	* synchronize on the given object.
	*
	* @param  lock
	*         Object to synchronize on
	*/
	@:overload private function new(lock : Dynamic) : Void;
	
	/**
	* Writes a single character.  The character to be written is contained in
	* the 16 low-order bits of the given integer value; the 16 high-order bits
	* are ignored.
	*
	* <p> Subclasses that intend to support efficient single-character output
	* should override this method.
	*
	* @param  c
	*         int specifying a character to be written
	*
	* @throws  IOException
	*          If an I/O error occurs
	*/
	@:overload public function write(c : Int) : Void;
	
	/**
	* Writes an array of characters.
	*
	* @param  cbuf
	*         Array of characters to be written
	*
	* @throws  IOException
	*          If an I/O error occurs
	*/
	@:overload public function write(cbuf : java.NativeArray<java.StdTypes.Char16>) : Void;
	
	/**
	* Writes a portion of an array of characters.
	*
	* @param  cbuf
	*         Array of characters
	*
	* @param  off
	*         Offset from which to start writing characters
	*
	* @param  len
	*         Number of characters to write
	*
	* @throws  IOException
	*          If an I/O error occurs
	*/
	@:overload @:abstract public function write(cbuf : java.NativeArray<java.StdTypes.Char16>, off : Int, len : Int) : Void;
	
	/**
	* Writes a string.
	*
	* @param  str
	*         String to be written
	*
	* @throws  IOException
	*          If an I/O error occurs
	*/
	@:overload public function write(str : String) : Void;
	
	/**
	* Writes a portion of a string.
	*
	* @param  str
	*         A String
	*
	* @param  off
	*         Offset from which to start writing characters
	*
	* @param  len
	*         Number of characters to write
	*
	* @throws  IndexOutOfBoundsException
	*          If <tt>off</tt> is negative, or <tt>len</tt> is negative,
	*          or <tt>off+len</tt> is negative or greater than the length
	*          of the given string
	*
	* @throws  IOException
	*          If an I/O error occurs
	*/
	@:overload public function write(str : String, off : Int, len : Int) : Void;
	
	/**
	* Appends the specified character sequence to this writer.
	*
	* <p> An invocation of this method of the form <tt>out.append(csq)</tt>
	* behaves in exactly the same way as the invocation
	*
	* <pre>
	*     out.write(csq.toString()) </pre>
	*
	* <p> Depending on the specification of <tt>toString</tt> for the
	* character sequence <tt>csq</tt>, the entire sequence may not be
	* appended. For instance, invoking the <tt>toString</tt> method of a
	* character buffer will return a subsequence whose content depends upon
	* the buffer's position and limit.
	*
	* @param  csq
	*         The character sequence to append.  If <tt>csq</tt> is
	*         <tt>null</tt>, then the four characters <tt>"null"</tt> are
	*         appended to this writer.
	*
	* @return  This writer
	*
	* @throws  IOException
	*          If an I/O error occurs
	*
	* @since  1.5
	*/
	@:require(java5) @:overload public function append(csq : java.lang.CharSequence) : Writer;
	
	/**
	* Appends a subsequence of the specified character sequence to this writer.
	* <tt>Appendable</tt>.
	*
	* <p> An invocation of this method of the form <tt>out.append(csq, start,
	* end)</tt> when <tt>csq</tt> is not <tt>null</tt> behaves in exactly the
	* same way as the invocation
	*
	* <pre>
	*     out.write(csq.subSequence(start, end).toString()) </pre>
	*
	* @param  csq
	*         The character sequence from which a subsequence will be
	*         appended.  If <tt>csq</tt> is <tt>null</tt>, then characters
	*         will be appended as if <tt>csq</tt> contained the four
	*         characters <tt>"null"</tt>.
	*
	* @param  start
	*         The index of the first character in the subsequence
	*
	* @param  end
	*         The index of the character following the last character in the
	*         subsequence
	*
	* @return  This writer
	*
	* @throws  IndexOutOfBoundsException
	*          If <tt>start</tt> or <tt>end</tt> are negative, <tt>start</tt>
	*          is greater than <tt>end</tt>, or <tt>end</tt> is greater than
	*          <tt>csq.length()</tt>
	*
	* @throws  IOException
	*          If an I/O error occurs
	*
	* @since  1.5
	*/
	@:require(java5) @:overload public function append(csq : java.lang.CharSequence, start : Int, end : Int) : Writer;
	
	/**
	* Appends the specified character to this writer.
	*
	* <p> An invocation of this method of the form <tt>out.append(c)</tt>
	* behaves in exactly the same way as the invocation
	*
	* <pre>
	*     out.write(c) </pre>
	*
	* @param  c
	*         The 16-bit character to append
	*
	* @return  This writer
	*
	* @throws  IOException
	*          If an I/O error occurs
	*
	* @since 1.5
	*/
	@:require(java5) @:overload public function append(c : java.StdTypes.Char16) : Writer;
	
	/**
	* Flushes the stream.  If the stream has saved any characters from the
	* various write() methods in a buffer, write them immediately to their
	* intended destination.  Then, if that destination is another character or
	* byte stream, flush it.  Thus one flush() invocation will flush all the
	* buffers in a chain of Writers and OutputStreams.
	*
	* <p> If the intended destination of this stream is an abstraction provided
	* by the underlying operating system, for example a file, then flushing the
	* stream guarantees only that bytes previously written to the stream are
	* passed to the operating system for writing; it does not guarantee that
	* they are actually written to a physical device such as a disk drive.
	*
	* @throws  IOException
	*          If an I/O error occurs
	*/
	@:overload @:abstract public function flush() : Void;
	
	/**
	* Closes the stream, flushing it first. Once the stream has been closed,
	* further write() or flush() invocations will cause an IOException to be
	* thrown. Closing a previously closed stream has no effect.
	*
	* @throws  IOException
	*          If an I/O error occurs
	*/
	@:overload @:abstract public function close() : Void;
	
	
}
