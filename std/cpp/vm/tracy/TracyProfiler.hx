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

package cpp.vm.tracy;

// Disable this guard for now.
// #if (!HXCPP_TRACY)
// #error "This class cannot be used without -D HXCPP_TRACY"
// #end


enum abstract PlotFormatType(cpp.UInt8) from cpp.UInt8 to cpp.UInt8 {
    var Number = 0;
    var Memory = 1;
    var Percentage = 2;
}

@:include('hx/TelemetryTracy.h')
extern class Native_TracyProfiler {
    /**
        Mark a frame. Call this at the end of each frame loop.
    **/
    @:native('::__hxcpp_tracy_framemark')
    public static function frameMark():Void;

    /**
        Print a message into Tracy's log.
    **/
    @:native('::__hxcpp_tracy_message')
    public static function message(_msg:String, ?_color:Int = 0x000000):Void;

    /**
        Tracy can collect additional information about the profiled application,
        which will be available in the trace description. 
        This can include data such as the source repository revision,
        the application’s environment (dev/prod), etc.
    **/
    @:native('::__hxcpp_tracy_message_app_info')
    public static function messageAppInfo(_info:String):Void;

    /**
        Plot a named value to tracy. This will generate a graph in the profiler for you.
    **/
    @:native('::__hxcpp_tracy_plot')
    public static function plot(_name:String, _val:cpp.Float32):Void;

    /**
        Configure how values are plotted and displayed.
    **/
    @:native('::__hxcpp_tracy_plot_config')
    public static function plotConfig(_name:String, _format:PlotFormatType, ?_step:Bool=false, ?_fill:Bool=false, ?_color:Int=0x000000):Void;

    /**
        Set a name for the current thread this function is called in. Supply an optional groupHint so threads become grouped in Tracy's UI.
    **/
    @:native('::__hxcpp_tracy_set_thread_name_and_group')
    public static function setThreadName(_name:String, ?_groupHint:Int=1):Void;

    /**
        Create a custom named scoped zone in your code. 
    **/
    @:native('HXCPP_TRACY_ZONE')
    public static function zoneScoped(_name:String):Void;
}

#if (scriptable || cppia)
    class Cppia_TracyProfiler {        
        @:inheritDoc(cpp.vm.tracy.Native_TracyProfiler.frameMark)
        public static function frameMark()
            Native_TracyProfiler.frameMark();

        @:inheritDoc(cpp.vm.tracy.Native_TracyProfiler.message)
        public static function message(_msg:String, ?_color:Int = 0x000000)
            Native_TracyProfiler.message(_msg, _color);

        @:inheritDoc(cpp.vm.tracy.Native_TracyProfiler.messageAppInfo)
        public static function messageAppInfo(_info:String)
            Native_TracyProfiler.messageAppInfo(_info);

        @:inheritDoc(cpp.vm.tracy.Native_TracyProfiler.plot)
        public static function plot(_name:String, _val:Float)
            Native_TracyProfiler.plot(_name, _val);

        @:inheritDoc(cpp.vm.tracy.Native_TracyProfiler.plotConfig)
        public static function plotConfig(_name:String, _format:PlotFormatType, ?_step:Bool=false, ?_fill:Bool=false, ?_color:Int=0x000000)
            Native_TracyProfiler.plotConfig(_name, _format, _step, _fill, _color);

        @:inheritDoc(cpp.vm.tracy.Native_TracyProfiler.setThreadName)
        public static function setThreadName(_name:String, ?_groupHint:Int=1)
            Native_TracyProfiler.setThreadName(_name, _groupHint);

        @:inheritDoc(cpp.vm.tracy.Native_TracyProfiler.zoneScoped)
        public static function zoneScoped(_name:String)
            Native_TracyProfiler.zoneScoped(_name);
    }

    typedef TracyProfiler = Cppia_TracyProfiler;
#else
    typedef TracyProfiler = Native_TracyProfiler;
#end
