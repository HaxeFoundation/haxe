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
package cpp.vm;


/**
 * Parameter describes a function parameter.  Instances of this class are
 * embedded in stack frame objects to describe the function parameters that
 * were used in the invocation of the function that defines that stack frame.
 **/
class Parameter
{
    public var name(default, null) : String;
    public var value(default, null) : Dynamic;

    public function new(name : String, value : Dynamic)
   {
        this.name = name;
        this.value = value;
   }
   }

/**
 * StackFrame describes one call stack frame.
 **/
class StackFrame
   {
    public var fileName(default, null) : String;
    public var lineNumber(default, null) : Int;
    public var className(default, null) : String;
    public var functionName(default, null) : String;
    public var parameters(default, null) : Array<Parameter>;

    public function new(fileName : String, lineNumber : Int,
                        className : String, functionName : String)
   {
        this.fileName = fileName;
        this.lineNumber = lineNumber;
        this.className = className;
        this.functionName = functionName;
        this.parameters = new Array<Parameter>();
   }
   }

/**
 * ThreadInfo describes the state of a single thread.
 **/
class ThreadInfo
   {
    public static inline var STATUS_RUNNING = 1;
    public static inline var STATUS_STOPPED_BREAK_IMMEDIATE = 2;
    public static inline var STATUS_STOPPED_BREAKPOINT = 3;
    public static inline var STATUS_STOPPED_UNCAUGHT_EXCEPTION = 4;
    public static inline var STATUS_STOPPED_CRITICAL_ERROR = 5;

    // 0 is never a valid thread number
    public var number(default, null) : Int;
    public var status(default, null) : Int;
    // If status is "stopped breakpoint", this is the breakpoint number
    public var breakpoint(default, null) : Int;
    // If status is "critical error", this describes the error
    public var criticalErrorDescription(default, null) : String;
    // Stack will be listed with the lowest frame first
    public var stack(default, null) : Array<StackFrame>;

    public function new(number : Int, status : Int, breakpoint : Int = -1,
                        criticalErrorDescription : String = null)
   {
        this.number = number;
        this.status = status;
        this.breakpoint = breakpoint;
        this.criticalErrorDescription = criticalErrorDescription;
        this.stack = new Array<StackFrame>();
   }
   }

/**
 * This class wraps the hxcpp C++ implementation to provide a Haxe interface
 * to the low level debugging features
 **/
class Debugger
   {
    public static inline var THREAD_CREATED = 1;
    public static inline var THREAD_TERMINATED = 2;
    public static inline var THREAD_STARTED = 3;
    public static inline var THREAD_STOPPED = 4;

    public static inline var STEP_INTO = 1;
    public static inline var STEP_OVER = 2;
    public static inline var STEP_OUT = 3;

    // This tagging value is returned by getStackVariableValue() and
    // setStackVariableValue if the requested value does not exist at the
    // requested stack frame
    public static var NONEXISTENT_VALUE = new String("NONEXISTENT_VALUE");
    // This tagging value is returned by getStackVariableValue and
    // setStackVariableValue if the stack variable that is being set is on a
    // thread that is running, in which case the set does not take place.
    public static var THREAD_NOT_STOPPED = new String("THREAD_NOT_STOPPED");

    /**
     * Sets the handler callback to be made when asynchronous events occur,
     * specifically, when threads are created, terminated, started, or
     * stopped.  The calling thread becomes the "debugger" thread, which means
     * that it will be discluded from any breakpoints and will not be reported
     * on by any thread reporting requests.
     *
     * Be aware that this callback is made asynchronously and possibly by
     * multiple threads simultaneously.
     *
     * Setting this to null prevents further callbacks.
     *
     * Throws a string exception if the program does not support debugging
     * because it was not compiled with the HXCPP_DEBUGGER flag set.
     *
     * @param handler is a function that will be called back by asynchronous
     *        thread events.  Note that this function is called directly from
     *        the thread experiencing the event and the handler should return
     *        quickly to avoid blocking the calling thread unnecessarily.
     *        The parameters to handler are:
     *          - threadNumber, the thread number of the event
     *          - event, one of THREAD_CREATED, THREAD_TERMINATED,
     *            THREAD_STARTED, or THREAD_STOPPED
     *          - stackFrame, the stack frame number at which the thread is stopped,
     *            undefined if event is not THREAD_STOPPED
     *          - className, the class name at which the thread is stopped,
     *            undefined if event is not THREAD_STOPPED
     *          - functionName, the function name at which the thread is
     *            stopped, undefined if event is not THREAD_STOPPED
     *          - fileName, the file name at which the thread is stopped,
     *            undefined if event is not THREAD_STOPPED
     *          - lineNumber, the line number at which the thread is stopped,
     *            undefined if event is not THREAD_STOPPED
     **/
    public static function setEventNotificationHandler(
             handler : Int -> Int -> Int -> String -> String -> String -> Int -> Void)
    {
        untyped __global__.__hxcpp_dbg_setEventNotificationHandler(handler);
    }

    /**
     * This can be called to turn off (and then back on) all stopping of
     * debugged threads temporarily.  It should only be used by classes that
     * actually implement the debugger to hide themselves from the debugger as
     * necessary.
     **/
    public static function enableCurrentThreadDebugging(enabled : Bool)
    {
        untyped __global__.__hxcpp_dbg_enableCurrentThreadDebugging(enabled);
    }

    /**
     * Returns the thread number of the calling thread.
     *
     * @return the thread number of the calling thread.
     **/
	public static function getCurrentThreadNumber() : Int
	{
        return untyped __global__.__hxcpp_dbg_getCurrentThreadNumber();
	}

    /**
     * Returns the set of source files known to the debugger.  This is a copy
     * of the original array and could be quite large.  The caller should
     * cache this value to avoid multiple copies needing to be made.
     *
     * @return the set of source files known to the debugger.
     **/
    public static function getFiles() : Array<String>
   {
        return untyped __global__.__hxcpp_dbg_getFiles();
   }


    /**
     * Returns the full paths of the set of source files known to the debugger.
     * This is a copy of the original array and could be quite large.
     * It is possible that this set will be empty, in which case the full paths are not known.
     * The index of these files matches the index from "getFiles", so the full path for
     * a given short path can be calculated.
     *
     * @return the known full paths of the set of source files
     **/
   public static function getFilesFullPath() : Array<String>
   {
        return untyped __global__.__hxcpp_dbg_getFilesFullPath();
   }

    /**
     * Returns the set of class names of all classes known to the debugger.
     * This is a copy of the original array and could be quite large.  The
     * caller should cache this value to avoid multiple copies needing to be
     * made.
     *
     * @return the set of class names of all classes known to the debugger.
     **/
    public static function getClasses() : Array<String>
    {
        return untyped __global__.__hxcpp_dbg_getClasses();
    }

    /**
     * Returns a ThreadInfo object describing every thread that existed at the
     * moment that the call was made, except for the debugger thread.
     **/
    public static function getThreadInfos() : Array<ThreadInfo>
    {
        return untyped __global__.__hxcpp_dbg_getThreadInfos();
    }

    /**
     * Returns a ThreadInfo object describing a single thread, or null if
     * there is no such thread or the thread queried about was the debugger
     * thread and unsafe was not true.
     **/
    public static function getThreadInfo(threadNumber : Int,
                                         unsafe : Bool) : ThreadInfo
    {
        return untyped __global__.__hxcpp_dbg_getThreadInfo
            (threadNumber, unsafe);
    }

    /**
     * Adds a new file:line breakpoint.  The breakpoint number of the newly
     * added breakpoint is returned.
     **/
    public static function addFileLineBreakpoint(file : String,
                                                 line : Int) : Int
    {
        return untyped __global__.__hxcpp_dbg_addFileLineBreakpoint
            (file, line);
    }

    /**
     * Adds a new class:function breakpoint.  The breakpoint number of the
     * newly added breakpoint is returned.
     **/
    public static function addClassFunctionBreakpoint(className : String,
                                                   functionName : String) : Int
    {
        return untyped __global__.__hxcpp_dbg_addClassFunctionBreakpoint
            (className, functionName);
    }

    /**
     * Deletes a breakpoint, or all breakpoints.
     **/
    public static function deleteBreakpoint(number : Null<Int>)
    {
        if (number == null) {
            untyped __global__.__hxcpp_dbg_deleteAllBreakpoints();
        }
        else {
            untyped __global__.__hxcpp_dbg_deleteBreakpoint
                (cast (number, Int));
        }
    }

    /**
     * Breaks all threads except the debugger thread (which should be the same
     * as the calling thread!).
     *
     * If `wait` is true, waits up to 2 seconds for all threads to be broken.
     * Threads which are in blocking system calls and cannot break after 2
     * seconds remain running when this function returns.
     **/
    public static function breakNow(wait : Bool = true)
    {
        untyped __global__.__hxcpp_dbg_breakNow(wait);
    }

    /**
     * Continue execution of all stopped threads.  If specialThreadNumber
     * is a valid thread number, then it will be continued past
     * `continueCount` breakpoints instead of just 1 like all of the other
     * threads.
     **/
    public static function continueThreads(specialThreadNumber : Int,
                                           continueCount : Int)
    {
        untyped __global__.__hxcpp_dbg_continueThreads
            (specialThreadNumber, continueCount);
    }

    /**
     * Single steps the given thread.
     **/
    public static function stepThread(threadNumber : Int,
                                      stepType : Int,
                                      stepCount : Int = 1)
    {
        untyped __global__.__hxcpp_dbg_stepThread
            (threadNumber, stepType, stepCount);
    }

    /**
     * Returns the list of local variables (including "this", function
     * arguments, and local variables) visible to the given thread at the
     * given stack frame.
     *
     * Returns a list with a single entry, THREAD_NOT_STOPPED, if the
     * thread is not stopped and thus variables cannot be fetched and
     * unsafe is not true.
     *
     * @return the list of local variables (including "this", function
     *         arguments, and local variables) visible to the given thread at
     *         the given stack frame.
     **/
    public static function getStackVariables(threadNumber : Int,
                                             stackFrameNumber : Int,
                                             unsafe : Bool) : Array<String>
    {
        return untyped __global__.__hxcpp_dbg_getStackVariables
            (threadNumber, stackFrameNumber, unsafe, THREAD_NOT_STOPPED);
    }

    /**
     * Returns the value of a stack variable, or NONEXISTENT_VALUE if the
     * requested value does not exist.  If the thread is actively running
     * and unsafe is not true, returns THREAD_NOT_STOPPED.
     **/
    public static function getStackVariableValue(threadNumber : Int,
                                                 stackFrameNumber : Int,
                                                 name : String,
                                                 unsafe : Bool) : Dynamic
    {
        return untyped __global__.__hxcpp_dbg_getStackVariableValue
            (threadNumber, stackFrameNumber, name, unsafe, NONEXISTENT_VALUE,
             THREAD_NOT_STOPPED);
    }

    /**
     * Sets the value of a stack variable and returns that value.  If the
     * variable does not exist, on the stack, this function returns
     * NONEXISTENT_VALUE.  If the thread is actively running and unsafe is not
     * true, returns THREAD_NOT_STOPPED, and the value is not set.
     **/
    public static function setStackVariableValue(threadNumber : Int,
                                                 stackFrameNumber : Int,
                                                 name : String,
                                                 value : Dynamic,
                                                 unsafe : Bool) : Dynamic
    {
        return untyped __global__.__hxcpp_dbg_setStackVariableValue
            (threadNumber, stackFrameNumber, name, value, unsafe,
             NONEXISTENT_VALUE, THREAD_NOT_STOPPED);
    }

    // The hxcpp runtime calls back through these functions to create Haxe
    // objects as needed, which allows the C++ implementation code to create
    // Haxe objects without having to actually know the structure of those
    // objects
    private static function __init__()
    {
        untyped __global__.__hxcpp_dbg_setNewParameterFunction
        (
         function (name : String, value : Dynamic) : Dynamic {
             return new Parameter(name, value);
         }
        );

        untyped __global__.__hxcpp_dbg_setNewStackFrameFunction
        (
         function (fileName : String, lineNumber : Int,
                   className : String, functionName : String)
         {
             return new StackFrame(fileName, lineNumber,
                                   className, functionName);
         }
        );

        untyped __global__.__hxcpp_dbg_setNewThreadInfoFunction
        (
         function (number : Int, status : Int, breakpoint : Int,
                   criticalErrorDescription : String) : Dynamic {
             return new ThreadInfo(number, status, breakpoint,
                                   criticalErrorDescription);
         }
        );

        untyped __global__.__hxcpp_dbg_setAddParameterToStackFrameFunction
        (
         function(inStackFrame : Dynamic, inParameter : Dynamic) {
             var stackFrame : StackFrame = cast inStackFrame;
             var parameter : Parameter = cast inParameter;
             stackFrame.parameters.push(parameter);
         }
        );

        untyped __global__.__hxcpp_dbg_setAddStackFrameToThreadInfoFunction
        (
         function(inThreadInfo : Dynamic, inStackFrame : Dynamic) {
             var threadInfo : ThreadInfo = cast inThreadInfo;
             var stackFrame : StackFrame = cast inStackFrame;
             threadInfo.stack.push(stackFrame);
   }
        );
   }
}

