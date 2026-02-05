package cs.system.diagnostics;

/** Provides access to local and remote processes and enables you to start and stop local system processes. */
@:native("System.Diagnostics.Process")
extern class Process extends cs.system.componentmodel.Component {
	/**
	 * Gets the base priority of the associated process.
	 * @return The base priority, which is computed from the  of the associated
	 * process.
	 */
	var BasePriority(default, never):Int;
	/**
	 * Gets or sets whether the  event should be raised when the process terminates.
	 * @return if the  event should be raised when the associated process is terminated
	 * (through either an exit or a call to ); otherwise, . The default is . Note that
	 * the  event is raised even if the value of  is  when the process exits during or
	 * before the user performs a  check.
	 */
	var EnableRaisingEvents(default, default):Bool;
	/**
	 * Gets the value that the associated process specified when it terminated.
	 * @return The code that the associated process specified when it terminated.
	 */
	var ExitCode(default, never):Int;
	/**
	 * Gets the time that the associated process exited.
	 * @return A  that indicates when the associated process was terminated.
	 */
	var ExitTime(default, never):cs.system.DateTime;
	/**
	 * Gets the native handle of the associated process.
	 * @return The handle that the operating system assigned to the associated process
	 * when the process was started. The system uses this handle to keep track of
	 * process attributes.
	 */
	var Handle(default, never):cs.system.IntPtr;
	/**
	 * Gets the number of handles opened by the process.
	 * @return The number of operating system handles the process has opened.
	 */
	var HandleCount(default, never):Int;
	/**
	 * Gets a value indicating whether the associated process has been terminated.
	 * @return if the operating system process referenced by the  component has
	 * terminated; otherwise, .
	 */
	var HasExited(default, never):Bool;
	/**
	 * Gets the unique identifier for the associated process.
	 * @return The system-generated unique identifier of the process that is referenced
	 * by this  instance.
	 */
	var Id(default, never):Int;
	/**
	 * Gets the name of the computer the associated process is running on.
	 * @return The name of the computer that the associated process is running on.
	 */
	var MachineName(default, never):String;
	/**
	 * Gets the main module for the associated process.
	 * @return The  that was used to start the process.
	 */
	var MainModule(default, never):cs.system.diagnostics.ProcessModule;
	/**
	 * Gets the window handle of the main window of the associated process.
	 * @return The system-generated window handle of the main window of the associated
	 * process.
	 */
	var MainWindowHandle(default, never):cs.system.IntPtr;
	/**
	 * Gets the caption of the main window of the process.
	 * @return The main window title of the process.
	 */
	var MainWindowTitle(default, never):String;
	/**
	 * Gets or sets the maximum allowable working set size, in bytes, for the
	 * associated process.
	 * @return The maximum working set size that is allowed in memory for the process,
	 * in bytes.
	 */
	var MaxWorkingSet(default, default):cs.system.IntPtr;
	/**
	 * Gets or sets the minimum allowable working set size, in bytes, for the
	 * associated process.
	 * @return The minimum working set size that is required in memory for the process,
	 * in bytes.
	 */
	var MinWorkingSet(default, default):cs.system.IntPtr;
	/**
	 * Gets the modules that have been loaded by the associated process.
	 * @return An array of type  that represents the modules that have been loaded by
	 * the associated process.
	 */
	var Modules(default, never):cs.system.diagnostics.ProcessModuleCollection;
	/**
	 * Gets the amount of nonpaged system memory, in bytes, allocated for the
	 * associated process.
	 * @return The amount of memory, in bytes, the system has allocated for the
	 * associated process that cannot be written to the virtual memory paging file.
	 */
	var NonpagedSystemMemorySize(default, never):Int;
	/**
	 * Gets the amount of nonpaged system memory, in bytes, allocated for the
	 * associated process.
	 * @return The amount of system memory, in bytes, allocated for the associated
	 * process that cannot be written to the virtual memory paging file.
	 */
	var NonpagedSystemMemorySize64(default, never):haxe.Int64;
	/**
	 * Gets the amount of paged memory, in bytes, allocated for the associated process.
	 * @return The amount of memory, in bytes, allocated by the associated process that
	 * can be written to the virtual memory paging file.
	 */
	var PagedMemorySize(default, never):Int;
	/**
	 * Gets the amount of paged memory, in bytes, allocated for the associated process.
	 * @return The amount of memory, in bytes, allocated in the virtual memory paging
	 * file for the associated process.
	 */
	var PagedMemorySize64(default, never):haxe.Int64;
	/**
	 * Gets the amount of pageable system memory, in bytes, allocated for the
	 * associated process.
	 * @return The amount of memory, in bytes, the system has allocated for the
	 * associated process that can be written to the virtual memory paging file.
	 */
	var PagedSystemMemorySize(default, never):Int;
	/**
	 * Gets the amount of pageable system memory, in bytes, allocated for the
	 * associated process.
	 * @return The amount of system memory, in bytes, allocated for the associated
	 * process that can be written to the virtual memory paging file.
	 */
	var PagedSystemMemorySize64(default, never):haxe.Int64;
	/**
	 * Gets the maximum amount of memory in the virtual memory paging file, in bytes,
	 * used by the associated process.
	 * @return The maximum amount of memory, in bytes, allocated by the associated
	 * process that could be written to the virtual memory paging file.
	 */
	var PeakPagedMemorySize(default, never):Int;
	/**
	 * Gets the maximum amount of memory in the virtual memory paging file, in bytes,
	 * used by the associated process.
	 * @return The maximum amount of memory, in bytes, allocated in the virtual memory
	 * paging file for the associated process since it was started.
	 */
	var PeakPagedMemorySize64(default, never):haxe.Int64;
	/**
	 * Gets the maximum amount of virtual memory, in bytes, used by the associated
	 * process.
	 * @return The maximum amount of virtual memory, in bytes, that the associated
	 * process has requested.
	 */
	var PeakVirtualMemorySize(default, never):Int;
	/**
	 * Gets the maximum amount of virtual memory, in bytes, used by the associated
	 * process.
	 * @return The maximum amount of virtual memory, in bytes, allocated for the
	 * associated process since it was started.
	 */
	var PeakVirtualMemorySize64(default, never):haxe.Int64;
	/**
	 * Gets the peak working set size for the associated process, in bytes.
	 * @return The maximum amount of physical memory that the associated process has
	 * required all at once, in bytes.
	 */
	var PeakWorkingSet(default, never):Int;
	/**
	 * Gets the maximum amount of physical memory, in bytes, used by the associated
	 * process.
	 * @return The maximum amount of physical memory, in bytes, allocated for the
	 * associated process since it was started.
	 */
	var PeakWorkingSet64(default, never):haxe.Int64;
	/**
	 * Gets or sets a value indicating whether the associated process priority should
	 * temporarily be boosted by the operating system when the main window has the
	 * focus.
	 * @return if dynamic boosting of the process priority should take place for a
	 * process when it is taken out of the wait state; otherwise, . The default is .
	 */
	var PriorityBoostEnabled(default, default):Bool;
	/**
	 * Gets or sets the overall priority category for the associated process.
	 * @return The priority category for the associated process, from which the  of the
	 * process is calculated.
	 */
	var PriorityClass(default, default):cs.system.diagnostics.ProcessPriorityClass;
	/**
	 * Gets the amount of private memory, in bytes, allocated for the associated
	 * process.
	 * @return The number of bytes allocated by the associated process that cannot be
	 * shared with other processes.
	 */
	var PrivateMemorySize(default, never):Int;
	/**
	 * Gets the amount of private memory, in bytes, allocated for the associated
	 * process.
	 * @return The amount of memory, in bytes, allocated for the associated process
	 * that cannot be shared with other processes.
	 */
	var PrivateMemorySize64(default, never):haxe.Int64;
	/**
	 * Gets the privileged processor time for this process.
	 * @return A  that indicates the amount of time that the process has spent running
	 * code inside the operating system core.
	 */
	var PrivilegedProcessorTime(default, never):cs.system.TimeSpan;
	/**
	 * Gets the name of the process.
	 * @return The name that the system uses to identify the process to the user.
	 */
	var ProcessName(default, never):String;
	/**
	 * Gets or sets the processors on which the threads in this process can be
	 * scheduled to run.
	 * @return A bitmask representing the processors that the threads in the associated
	 * process can run on. The default depends on the number of processors on the
	 * computer. The default value is 2 n -1, where n is the number of processors.
	 */
	var ProcessorAffinity(default, default):cs.system.IntPtr;
	/**
	 * Gets a value indicating whether the user interface of the process is responding.
	 * @return if the user interface of the associated process is responding to the
	 * system; otherwise, .
	 */
	var Responding(default, never):Bool;
	/**
	 * Gets the native handle to this process.
	 * @return The native handle to this process.
	 */
	var SafeHandle(default, never):cs.microsoft.win32.safehandles.SafeProcessHandle;
	/**
	 * Gets the Terminal Services session identifier for the associated process.
	 * @return The Terminal Services session identifier for the associated process.
	 */
	var SessionId(default, never):Int;
	/**
	 * Gets a stream used to read the error output of the application.
	 * @return A  that can be used to read the standard error stream of the
	 * application.
	 */
	var StandardError(default, never):cs.system.io.StreamReader;
	/**
	 * Gets a stream used to write the input of the application.
	 * @return A  that can be used to write the standard input stream of the
	 * application.
	 */
	var StandardInput(default, never):cs.system.io.StreamWriter;
	/**
	 * Gets a stream used to read the textual output of the application.
	 * @return A  that can be used to read the standard output stream of the
	 * application.
	 */
	var StandardOutput(default, never):cs.system.io.StreamReader;
	/**
	 * Gets or sets the properties to pass to the  method of the .
	 * @return The  that represents the data with which to start the process. These
	 * arguments include the name of the executable file or document used to start the
	 * process.
	 */
	var StartInfo(default, default):cs.system.diagnostics.ProcessStartInfo;
	/**
	 * Gets the time that the associated process was started.
	 * @return An object  that indicates when the process started. An exception is
	 * thrown if the process is not running.
	 */
	var StartTime(default, never):cs.system.DateTime;
	/**
	 * Gets or sets the object used to marshal the event handler calls that are issued
	 * as a result of a process exit event.
	 * @return The  used to marshal event handler calls that are issued as a result of
	 * an  event on the process.
	 */
	var SynchronizingObject(default, default):cs.system.componentmodel.ISynchronizeInvoke;
	/**
	 * Gets the set of threads that are running in the associated process.
	 * @return An array of type  representing the operating system threads currently
	 * running in the associated process.
	 */
	var Threads(default, never):cs.system.diagnostics.ProcessThreadCollection;
	/**
	 * Gets the total processor time for this process.
	 * @return A  that indicates the amount of time that the associated process has
	 * spent utilizing the CPU. This value is the sum of the  and the .
	 */
	var TotalProcessorTime(default, never):cs.system.TimeSpan;
	/**
	 * Gets the user processor time for this process.
	 * @return A  that indicates the amount of time that the associated process has
	 * spent running code inside the application portion of the process (not inside the
	 * operating system core).
	 */
	var UserProcessorTime(default, never):cs.system.TimeSpan;
	/**
	 * Gets the size of the process's virtual memory, in bytes.
	 * @return The amount of virtual memory, in bytes, that the associated process has
	 * requested.
	 */
	var VirtualMemorySize(default, never):Int;
	/**
	 * Gets the amount of the virtual memory, in bytes, allocated for the associated
	 * process.
	 * @return The amount of virtual memory, in bytes, allocated for the associated
	 * process.
	 */
	var VirtualMemorySize64(default, never):haxe.Int64;
	/**
	 * Gets the associated process's physical memory usage, in bytes.
	 * @return The total amount of physical memory the associated process is using, in
	 * bytes.
	 */
	var WorkingSet(default, never):Int;
	/**
	 * Gets the amount of physical memory, in bytes, allocated for the associated
	 * process.
	 * @return The amount of physical memory, in bytes, allocated for the associated
	 * process.
	 */
	var WorkingSet64(default, never):haxe.Int64;
	function new():Void;
	/** Puts a  component in state to interact with operating system processes that run in a special mode by enabling the native property  on the current thread. */
	static function EnterDebugMode():Void;
	/**
	 * Gets a new  component and associates it with the currently active process.
	 * @return A new  component associated with the process resource that is running
	 * the calling application.
	 */
	static function GetCurrentProcess():cs.system.diagnostics.Process;
	@:overload(function(processId:Int):cs.system.diagnostics.Process {})
	/**
	 * Returns a new  component, given the identifier of a process on the local
	 * computer.
	 * @param processId The system-unique identifier of a process resource.
	 * @return A  component that is associated with the local process resource
	 * identified by the  parameter.
	 */
	static function GetProcessById(processId:Int, machineName:String):cs.system.diagnostics.Process;
	@:overload(function():cs.NativeArray<cs.system.diagnostics.Process> {})
	/**
	 * Creates a new  component for each process resource on the local computer.
	 * @return An array of type  that represents all the process resources running on
	 * the local computer.
	 */
	static function GetProcesses(machineName:String):cs.NativeArray<cs.system.diagnostics.Process>;
	@:overload(function(processName:String):cs.NativeArray<cs.system.diagnostics.Process> {})
	/**
	 * Creates an array of new  components and associates them with all the process
	 * resources on the local computer that share the specified process name.
	 * @param processName The friendly name of the process.
	 * @return An array of type  that represents the process resources running the
	 * specified application or file.
	 */
	static function GetProcessesByName(processName:String, machineName:String):cs.NativeArray<cs.system.diagnostics.Process>;
	/** Takes a  component out of the state that lets it interact with operating system processes that run in a special mode. */
	static function LeaveDebugMode():Void;
	@:overload(function(startInfo:cs.system.diagnostics.ProcessStartInfo):cs.system.diagnostics.Process {})
	@:overload(function(fileName:String):cs.system.diagnostics.Process {})
	@:overload(function(fileName:String, arguments:String):cs.system.diagnostics.Process {})
	@:overload(function(fileName:String, userName:String, password:cs.system.security.SecureString, domain:String):cs.system.diagnostics.Process {})
	/**
	 * Starts (or reuses) the process resource that is specified by the  property of
	 * this  component and associates it with the component.
	 * @return if a process resource is started;  if no new process resource is started
	 * (for example, if an existing process is reused).
	 */
	static function Start(fileName:String, arguments:String, userName:String, password:cs.system.security.SecureString, domain:String):cs.system.diagnostics.Process;
	/** Begins asynchronous read operations on the redirected  stream of the application. */
	function BeginErrorReadLine():Void;
	/** Begins asynchronous read operations on the redirected  stream of the application. */
	function BeginOutputReadLine():Void;
	/** Cancels the asynchronous read operation on the redirected  stream of an application. */
	function CancelErrorRead():Void;
	/** Cancels the asynchronous read operation on the redirected  stream of an application. */
	function CancelOutputRead():Void;
	/** Frees all the resources that are associated with this component. */
	function Close():Void;
	/**
	 * Closes a process that has a user interface by sending a close message to its
	 * main window.
	 * @return if the close message was successfully sent;  if the associated process
	 * does not have a main window or if the main window is disabled (for example if a
	 * modal dialog is being shown).
	 */
	function CloseMainWindow():Bool;
	/** Immediately stops the associated process. */
	function Kill():Void;
	/** Discards any information about the associated process that has been cached inside the process component. */
	function Refresh():Void;
	/**
	 * Starts (or reuses) the process resource that is specified by the  property of
	 * this  component and associates it with the component.
	 * @return if a process resource is started;  if no new process resource is started
	 * (for example, if an existing process is reused).
	 */
	function Start():Bool;
	/**
	 * Formats the process's name as a string, combined with the parent component type,
	 * if applicable.
	 * @return The , combined with the base component's  return value.
	 */
	function ToString():String;
	@:overload(function():Void {})
	/** Instructs the  component to wait indefinitely for the associated process to exit. */
	function WaitForExit(milliseconds:Int):Bool;
	@:overload(function():Bool {})
	/**
	 * Causes the  component to wait indefinitely for the associated process to enter
	 * an idle state. This overload applies only to processes with a user interface
	 * and, therefore, a message loop.
	 * @return if the associated process has reached an idle state.
	 */
	function WaitForInputIdle(milliseconds:Int):Bool;
}
