package java.io;
/*
* Copyright (c) 1996, 2011, Oracle and/or its affiliates. All rights reserved.
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
* Prints formatted representations of objects to a text-output stream.  This
* class implements all of the <tt>print</tt> methods found in {@link
* PrintStream}.  It does not contain methods for writing raw bytes, for which
* a program should use unencoded byte streams.
*
* <p> Unlike the {@link PrintStream} class, if automatic flushing is enabled
* it will be done only when one of the <tt>println</tt>, <tt>printf</tt>, or
* <tt>format</tt> methods is invoked, rather than whenever a newline character
* happens to be output.  These methods use the platform's own notion of line
* separator rather than the newline character.
*
* <p> Methods in this class never throw I/O exceptions, although some of its
* constructors may.  The client may inquire as to whether any errors have
* occurred by invoking {@link #checkError checkError()}.
*
* @author      Frank Yellin
* @author      Mark Reinhold
* @since       JDK1.1
*/
@:require(java1) extern class PrintWriter extends java.io.Writer
{
	/**
	* The underlying character-output stream of this
	* <code>PrintWriter</code>.
	*
	* @since 1.2
	*/
	@:require(java2) private var out : java.io.Writer;
	
	/**
	* Creates a new PrintWriter, without automatic line flushing.
	*
	* @param  out        A character-output stream
	*/
	@:overload public function new(out : java.io.Writer) : Void;
	
	/**
	* Creates a new PrintWriter.
	*
	* @param  out        A character-output stream
	* @param  autoFlush  A boolean; if true, the <tt>println</tt>,
	*                    <tt>printf</tt>, or <tt>format</tt> methods will
	*                    flush the output buffer
	*/
	@:overload public function new(out : java.io.Writer, autoFlush : Bool) : Void;
	
	/**
	* Creates a new PrintWriter, without automatic line flushing, from an
	* existing OutputStream.  This convenience constructor creates the
	* necessary intermediate OutputStreamWriter, which will convert characters
	* into bytes using the default character encoding.
	*
	* @param  out        An output stream
	*
	* @see java.io.OutputStreamWriter#OutputStreamWriter(java.io.OutputStream)
	*/
	@:overload public function new(out : java.io.OutputStream) : Void;
	
	/**
	* Creates a new PrintWriter from an existing OutputStream.  This
	* convenience constructor creates the necessary intermediate
	* OutputStreamWriter, which will convert characters into bytes using the
	* default character encoding.
	*
	* @param  out        An output stream
	* @param  autoFlush  A boolean; if true, the <tt>println</tt>,
	*                    <tt>printf</tt>, or <tt>format</tt> methods will
	*                    flush the output buffer
	*
	* @see java.io.OutputStreamWriter#OutputStreamWriter(java.io.OutputStream)
	*/
	@:overload public function new(out : java.io.OutputStream, autoFlush : Bool) : Void;
	
	/**
	* Creates a new PrintWriter, without automatic line flushing, with the
	* specified file name.  This convenience constructor creates the necessary
	* intermediate {@link java.io.OutputStreamWriter OutputStreamWriter},
	* which will encode characters using the {@linkplain
	* java.nio.charset.Charset#defaultCharset() default charset} for this
	* instance of the Java virtual machine.
	*
	* @param  fileName
	*         The name of the file to use as the destination of this writer.
	*         If the file exists then it will be truncated to zero size;
	*         otherwise, a new file will be created.  The output will be
	*         written to the file and is buffered.
	*
	* @throws  FileNotFoundException
	*          If the given string does not denote an existing, writable
	*          regular file and a new regular file of that name cannot be
	*          created, or if some other error occurs while opening or
	*          creating the file
	*
	* @throws  SecurityException
	*          If a security manager is present and {@link
	*          SecurityManager#checkWrite checkWrite(fileName)} denies write
	*          access to the file
	*
	* @since  1.5
	*/
	@:require(java5) @:overload public function new(fileName : String) : Void;
	
	/**
	* Creates a new PrintWriter, without automatic line flushing, with the
	* specified file name and charset.  This convenience constructor creates
	* the necessary intermediate {@link java.io.OutputStreamWriter
	* OutputStreamWriter}, which will encode characters using the provided
	* charset.
	*
	* @param  fileName
	*         The name of the file to use as the destination of this writer.
	*         If the file exists then it will be truncated to zero size;
	*         otherwise, a new file will be created.  The output will be
	*         written to the file and is buffered.
	*
	* @param  csn
	*         The name of a supported {@linkplain java.nio.charset.Charset
	*         charset}
	*
	* @throws  FileNotFoundException
	*          If the given string does not denote an existing, writable
	*          regular file and a new regular file of that name cannot be
	*          created, or if some other error occurs while opening or
	*          creating the file
	*
	* @throws  SecurityException
	*          If a security manager is present and {@link
	*          SecurityManager#checkWrite checkWrite(fileName)} denies write
	*          access to the file
	*
	* @throws  UnsupportedEncodingException
	*          If the named charset is not supported
	*
	* @since  1.5
	*/
	@:require(java5) @:overload public function new(fileName : String, csn : String) : Void;
	
	/**
	* Creates a new PrintWriter, without automatic line flushing, with the
	* specified file.  This convenience constructor creates the necessary
	* intermediate {@link java.io.OutputStreamWriter OutputStreamWriter},
	* which will encode characters using the {@linkplain
	* java.nio.charset.Charset#defaultCharset() default charset} for this
	* instance of the Java virtual machine.
	*
	* @param  file
	*         The file to use as the destination of this writer.  If the file
	*         exists then it will be truncated to zero size; otherwise, a new
	*         file will be created.  The output will be written to the file
	*         and is buffered.
	*
	* @throws  FileNotFoundException
	*          If the given file object does not denote an existing, writable
	*          regular file and a new regular file of that name cannot be
	*          created, or if some other error occurs while opening or
	*          creating the file
	*
	* @throws  SecurityException
	*          If a security manager is present and {@link
	*          SecurityManager#checkWrite checkWrite(file.getPath())}
	*          denies write access to the file
	*
	* @since  1.5
	*/
	@:require(java5) @:overload public function new(file : java.io.File) : Void;
	
	/**
	* Creates a new PrintWriter, without automatic line flushing, with the
	* specified file and charset.  This convenience constructor creates the
	* necessary intermediate {@link java.io.OutputStreamWriter
	* OutputStreamWriter}, which will encode characters using the provided
	* charset.
	*
	* @param  file
	*         The file to use as the destination of this writer.  If the file
	*         exists then it will be truncated to zero size; otherwise, a new
	*         file will be created.  The output will be written to the file
	*         and is buffered.
	*
	* @param  csn
	*         The name of a supported {@linkplain java.nio.charset.Charset
	*         charset}
	*
	* @throws  FileNotFoundException
	*          If the given file object does not denote an existing, writable
	*          regular file and a new regular file of that name cannot be
	*          created, or if some other error occurs while opening or
	*          creating the file
	*
	* @throws  SecurityException
	*          If a security manager is present and {@link
	*          SecurityManager#checkWrite checkWrite(file.getPath())}
	*          denies write access to the file
	*
	* @throws  UnsupportedEncodingException
	*          If the named charset is not supported
	*
	* @since  1.5
	*/
	@:require(java5) @:overload public function new(file : java.io.File, csn : String) : Void;
	
	/**
	* Flushes the stream.
	* @see #checkError()
	*/
	@:overload override public function flush() : Void;
	
	/**
	* Closes the stream and releases any system resources associated
	* with it. Closing a previously closed stream has no effect.
	*
	* @see #checkError()
	*/
	@:overload override public function close() : Void;
	
	/**
	* Flushes the stream if it's not closed and checks its error state.
	*
	* @return <code>true</code> if the print stream has encountered an error,
	*          either on the underlying output stream or during a format
	*          conversion.
	*/
	@:overload public function checkError() : Bool;
	
	/**
	* Indicates that an error has occurred.
	*
	* <p> This method will cause subsequent invocations of {@link
	* #checkError()} to return <tt>true</tt> until {@link
	* #clearError()} is invoked.
	*/
	@:overload private function setError() : Void;
	
	/**
	* Clears the error state of this stream.
	*
	* <p> This method will cause subsequent invocations of {@link
	* #checkError()} to return <tt>false</tt> until another write
	* operation fails and invokes {@link #setError()}.
	*
	* @since 1.6
	*/
	@:require(java6) @:overload private function clearError() : Void;
	
	/**
	* Writes a single character.
	* @param c int specifying a character to be written.
	*/
	@:overload override public function write(c : Int) : Void;
	
	/**
	* Writes A Portion of an array of characters.
	* @param buf Array of characters
	* @param off Offset from which to start writing characters
	* @param len Number of characters to write
	*/
	@:overload override public function write(buf : java.NativeArray<java.StdTypes.Char16>, off : Int, len : Int) : Void;
	
	/**
	* Writes an array of characters.  This method cannot be inherited from the
	* Writer class because it must suppress I/O exceptions.
	* @param buf Array of characters to be written
	*/
	@:overload override public function write(buf : java.NativeArray<java.StdTypes.Char16>) : Void;
	
	/**
	* Writes a portion of a string.
	* @param s A String
	* @param off Offset from which to start writing characters
	* @param len Number of characters to write
	*/
	@:overload override public function write(s : String, off : Int, len : Int) : Void;
	
	/**
	* Writes a string.  This method cannot be inherited from the Writer class
	* because it must suppress I/O exceptions.
	* @param s String to be written
	*/
	@:overload override public function write(s : String) : Void;
	
	/**
	* Prints a boolean value.  The string produced by <code>{@link
	* java.lang.String#valueOf(boolean)}</code> is translated into bytes
	* according to the platform's default character encoding, and these bytes
	* are written in exactly the manner of the <code>{@link
	* #write(int)}</code> method.
	*
	* @param      b   The <code>boolean</code> to be printed
	*/
	@:overload public function print(b : Bool) : Void;
	
	/**
	* Prints a character.  The character is translated into one or more bytes
	* according to the platform's default character encoding, and these bytes
	* are written in exactly the manner of the <code>{@link
	* #write(int)}</code> method.
	*
	* @param      c   The <code>char</code> to be printed
	*/
	@:overload public function print(c : java.StdTypes.Char16) : Void;
	
	/**
	* Prints an integer.  The string produced by <code>{@link
	* java.lang.String#valueOf(int)}</code> is translated into bytes according
	* to the platform's default character encoding, and these bytes are
	* written in exactly the manner of the <code>{@link #write(int)}</code>
	* method.
	*
	* @param      i   The <code>int</code> to be printed
	* @see        java.lang.Integer#toString(int)
	*/
	@:overload public function print(i : Int) : Void;
	
	/**
	* Prints a long integer.  The string produced by <code>{@link
	* java.lang.String#valueOf(long)}</code> is translated into bytes
	* according to the platform's default character encoding, and these bytes
	* are written in exactly the manner of the <code>{@link #write(int)}</code>
	* method.
	*
	* @param      l   The <code>long</code> to be printed
	* @see        java.lang.Long#toString(long)
	*/
	@:overload public function print(l : haxe.Int64) : Void;
	
	/**
	* Prints a floating-point number.  The string produced by <code>{@link
	* java.lang.String#valueOf(float)}</code> is translated into bytes
	* according to the platform's default character encoding, and these bytes
	* are written in exactly the manner of the <code>{@link #write(int)}</code>
	* method.
	*
	* @param      f   The <code>float</code> to be printed
	* @see        java.lang.Float#toString(float)
	*/
	@:overload public function print(f : Single) : Void;
	
	/**
	* Prints a double-precision floating-point number.  The string produced by
	* <code>{@link java.lang.String#valueOf(double)}</code> is translated into
	* bytes according to the platform's default character encoding, and these
	* bytes are written in exactly the manner of the <code>{@link
	* #write(int)}</code> method.
	*
	* @param      d   The <code>double</code> to be printed
	* @see        java.lang.Double#toString(double)
	*/
	@:overload public function print(d : Float) : Void;
	
	/**
	* Prints an array of characters.  The characters are converted into bytes
	* according to the platform's default character encoding, and these bytes
	* are written in exactly the manner of the <code>{@link #write(int)}</code>
	* method.
	*
	* @param      s   The array of chars to be printed
	*
	* @throws  NullPointerException  If <code>s</code> is <code>null</code>
	*/
	@:overload public function print(s : java.NativeArray<java.StdTypes.Char16>) : Void;
	
	/**
	* Prints a string.  If the argument is <code>null</code> then the string
	* <code>"null"</code> is printed.  Otherwise, the string's characters are
	* converted into bytes according to the platform's default character
	* encoding, and these bytes are written in exactly the manner of the
	* <code>{@link #write(int)}</code> method.
	*
	* @param      s   The <code>String</code> to be printed
	*/
	@:overload public function print(s : String) : Void;
	
	/**
	* Prints an object.  The string produced by the <code>{@link
	* java.lang.String#valueOf(Object)}</code> method is translated into bytes
	* according to the platform's default character encoding, and these bytes
	* are written in exactly the manner of the <code>{@link #write(int)}</code>
	* method.
	*
	* @param      obj   The <code>Object</code> to be printed
	* @see        java.lang.Object#toString()
	*/
	@:overload public function print(obj : Dynamic) : Void;
	
	/**
	* Terminates the current line by writing the line separator string.  The
	* line separator string is defined by the system property
	* <code>line.separator</code>, and is not necessarily a single newline
	* character (<code>'\n'</code>).
	*/
	@:overload public function println() : Void;
	
	/**
	* Prints a boolean value and then terminates the line.  This method behaves
	* as though it invokes <code>{@link #print(boolean)}</code> and then
	* <code>{@link #println()}</code>.
	*
	* @param x the <code>boolean</code> value to be printed
	*/
	@:overload public function println(x : Bool) : Void;
	
	/**
	* Prints a character and then terminates the line.  This method behaves as
	* though it invokes <code>{@link #print(char)}</code> and then <code>{@link
	* #println()}</code>.
	*
	* @param x the <code>char</code> value to be printed
	*/
	@:overload public function println(x : java.StdTypes.Char16) : Void;
	
	/**
	* Prints an integer and then terminates the line.  This method behaves as
	* though it invokes <code>{@link #print(int)}</code> and then <code>{@link
	* #println()}</code>.
	*
	* @param x the <code>int</code> value to be printed
	*/
	@:overload public function println(x : Int) : Void;
	
	/**
	* Prints a long integer and then terminates the line.  This method behaves
	* as though it invokes <code>{@link #print(long)}</code> and then
	* <code>{@link #println()}</code>.
	*
	* @param x the <code>long</code> value to be printed
	*/
	@:overload public function println(x : haxe.Int64) : Void;
	
	/**
	* Prints a floating-point number and then terminates the line.  This method
	* behaves as though it invokes <code>{@link #print(float)}</code> and then
	* <code>{@link #println()}</code>.
	*
	* @param x the <code>float</code> value to be printed
	*/
	@:overload public function println(x : Single) : Void;
	
	/**
	* Prints a double-precision floating-point number and then terminates the
	* line.  This method behaves as though it invokes <code>{@link
	* #print(double)}</code> and then <code>{@link #println()}</code>.
	*
	* @param x the <code>double</code> value to be printed
	*/
	@:overload public function println(x : Float) : Void;
	
	/**
	* Prints an array of characters and then terminates the line.  This method
	* behaves as though it invokes <code>{@link #print(char[])}</code> and then
	* <code>{@link #println()}</code>.
	*
	* @param x the array of <code>char</code> values to be printed
	*/
	@:overload public function println(x : java.NativeArray<java.StdTypes.Char16>) : Void;
	
	/**
	* Prints a String and then terminates the line.  This method behaves as
	* though it invokes <code>{@link #print(String)}</code> and then
	* <code>{@link #println()}</code>.
	*
	* @param x the <code>String</code> value to be printed
	*/
	@:overload public function println(x : String) : Void;
	
	/**
	* Prints an Object and then terminates the line.  This method calls
	* at first String.valueOf(x) to get the printed object's string value,
	* then behaves as
	* though it invokes <code>{@link #print(String)}</code> and then
	* <code>{@link #println()}</code>.
	*
	* @param x  The <code>Object</code> to be printed.
	*/
	@:overload public function println(x : Dynamic) : Void;
	
	/**
	* A convenience method to write a formatted string to this writer using
	* the specified format string and arguments.  If automatic flushing is
	* enabled, calls to this method will flush the output buffer.
	*
	* <p> An invocation of this method of the form <tt>out.printf(format,
	* args)</tt> behaves in exactly the same way as the invocation
	*
	* <pre>
	*     out.format(format, args) </pre>
	*
	* @param  format
	*         A format string as described in <a
	*         href="../util/Formatter.html#syntax">Format string syntax</a>.
	*
	* @param  args
	*         Arguments referenced by the format specifiers in the format
	*         string.  If there are more arguments than format specifiers, the
	*         extra arguments are ignored.  The number of arguments is
	*         variable and may be zero.  The maximum number of arguments is
	*         limited by the maximum dimension of a Java array as defined by
	*         <cite>The Java&trade; Virtual Machine Specification</cite>.
	*         The behaviour on a
	*         <tt>null</tt> argument depends on the <a
	*         href="../util/Formatter.html#syntax">conversion</a>.
	*
	* @throws  IllegalFormatException
	*          If a format string contains an illegal syntax, a format
	*          specifier that is incompatible with the given arguments,
	*          insufficient arguments given the format string, or other
	*          illegal conditions.  For specification of all possible
	*          formatting errors, see the <a
	*          href="../util/Formatter.html#detail">Details</a> section of the
	*          formatter class specification.
	*
	* @throws  NullPointerException
	*          If the <tt>format</tt> is <tt>null</tt>
	*
	* @return  This writer
	*
	* @since  1.5
	*/
	@:require(java5) @:overload public function printf(format : String, args : java.NativeArray<Dynamic>) : PrintWriter;
	
	/**
	* A convenience method to write a formatted string to this writer using
	* the specified format string and arguments.  If automatic flushing is
	* enabled, calls to this method will flush the output buffer.
	*
	* <p> An invocation of this method of the form <tt>out.printf(l, format,
	* args)</tt> behaves in exactly the same way as the invocation
	*
	* <pre>
	*     out.format(l, format, args) </pre>
	*
	* @param  l
	*         The {@linkplain java.util.Locale locale} to apply during
	*         formatting.  If <tt>l</tt> is <tt>null</tt> then no localization
	*         is applied.
	*
	* @param  format
	*         A format string as described in <a
	*         href="../util/Formatter.html#syntax">Format string syntax</a>.
	*
	* @param  args
	*         Arguments referenced by the format specifiers in the format
	*         string.  If there are more arguments than format specifiers, the
	*         extra arguments are ignored.  The number of arguments is
	*         variable and may be zero.  The maximum number of arguments is
	*         limited by the maximum dimension of a Java array as defined by
	*         <cite>The Java&trade; Virtual Machine Specification</cite>.
	*         The behaviour on a
	*         <tt>null</tt> argument depends on the <a
	*         href="../util/Formatter.html#syntax">conversion</a>.
	*
	* @throws  IllegalFormatException
	*          If a format string contains an illegal syntax, a format
	*          specifier that is incompatible with the given arguments,
	*          insufficient arguments given the format string, or other
	*          illegal conditions.  For specification of all possible
	*          formatting errors, see the <a
	*          href="../util/Formatter.html#detail">Details</a> section of the
	*          formatter class specification.
	*
	* @throws  NullPointerException
	*          If the <tt>format</tt> is <tt>null</tt>
	*
	* @return  This writer
	*
	* @since  1.5
	*/
	@:require(java5) @:overload public function printf(l : java.util.Locale, format : String, args : java.NativeArray<Dynamic>) : PrintWriter;
	
	/**
	* Writes a formatted string to this writer using the specified format
	* string and arguments.  If automatic flushing is enabled, calls to this
	* method will flush the output buffer.
	*
	* <p> The locale always used is the one returned by {@link
	* java.util.Locale#getDefault() Locale.getDefault()}, regardless of any
	* previous invocations of other formatting methods on this object.
	*
	* @param  format
	*         A format string as described in <a
	*         href="../util/Formatter.html#syntax">Format string syntax</a>.
	*
	* @param  args
	*         Arguments referenced by the format specifiers in the format
	*         string.  If there are more arguments than format specifiers, the
	*         extra arguments are ignored.  The number of arguments is
	*         variable and may be zero.  The maximum number of arguments is
	*         limited by the maximum dimension of a Java array as defined by
	*         <cite>The Java&trade; Virtual Machine Specification</cite>.
	*         The behaviour on a
	*         <tt>null</tt> argument depends on the <a
	*         href="../util/Formatter.html#syntax">conversion</a>.
	*
	* @throws  IllegalFormatException
	*          If a format string contains an illegal syntax, a format
	*          specifier that is incompatible with the given arguments,
	*          insufficient arguments given the format string, or other
	*          illegal conditions.  For specification of all possible
	*          formatting errors, see the <a
	*          href="../util/Formatter.html#detail">Details</a> section of the
	*          Formatter class specification.
	*
	* @throws  NullPointerException
	*          If the <tt>format</tt> is <tt>null</tt>
	*
	* @return  This writer
	*
	* @since  1.5
	*/
	@:require(java5) @:overload public function format(format : String, args : java.NativeArray<Dynamic>) : PrintWriter;
	
	/**
	* Writes a formatted string to this writer using the specified format
	* string and arguments.  If automatic flushing is enabled, calls to this
	* method will flush the output buffer.
	*
	* @param  l
	*         The {@linkplain java.util.Locale locale} to apply during
	*         formatting.  If <tt>l</tt> is <tt>null</tt> then no localization
	*         is applied.
	*
	* @param  format
	*         A format string as described in <a
	*         href="../util/Formatter.html#syntax">Format string syntax</a>.
	*
	* @param  args
	*         Arguments referenced by the format specifiers in the format
	*         string.  If there are more arguments than format specifiers, the
	*         extra arguments are ignored.  The number of arguments is
	*         variable and may be zero.  The maximum number of arguments is
	*         limited by the maximum dimension of a Java array as defined by
	*         <cite>The Java&trade; Virtual Machine Specification</cite>.
	*         The behaviour on a
	*         <tt>null</tt> argument depends on the <a
	*         href="../util/Formatter.html#syntax">conversion</a>.
	*
	* @throws  IllegalFormatException
	*          If a format string contains an illegal syntax, a format
	*          specifier that is incompatible with the given arguments,
	*          insufficient arguments given the format string, or other
	*          illegal conditions.  For specification of all possible
	*          formatting errors, see the <a
	*          href="../util/Formatter.html#detail">Details</a> section of the
	*          formatter class specification.
	*
	* @throws  NullPointerException
	*          If the <tt>format</tt> is <tt>null</tt>
	*
	* @return  This writer
	*
	* @since  1.5
	*/
	@:require(java5) @:overload public function format(l : java.util.Locale, format : String, args : java.NativeArray<Dynamic>) : PrintWriter;
	
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
	* @since  1.5
	*/
	@:require(java5) @:overload override public function append(csq : java.lang.CharSequence) : PrintWriter;
	
	/**
	* Appends a subsequence of the specified character sequence to this writer.
	*
	* <p> An invocation of this method of the form <tt>out.append(csq, start,
	* end)</tt> when <tt>csq</tt> is not <tt>null</tt>, behaves in
	* exactly the same way as the invocation
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
	* @since  1.5
	*/
	@:require(java5) @:overload override public function append(csq : java.lang.CharSequence, start : Int, end : Int) : PrintWriter;
	
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
	* @since 1.5
	*/
	@:require(java5) @:overload override public function append(c : java.StdTypes.Char16) : PrintWriter;
	
	
}
