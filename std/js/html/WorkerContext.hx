/*
 * Copyright (C)2005-2012 Haxe Foundation
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

// This file is generated, do not edit!
package js.html;

@:native("WorkerContext")
extern class WorkerContext extends EventTarget
{
    static inline var PERSISTENT :Int = 1;

    static inline var TEMPORARY :Int = 0;

    var indexedDB (default,null) :js.html.idb.Factory;

    var location (default,null) :WorkerLocation;

    var navigator (default,null) :WorkerNavigator;

    var onerror :EventListener;

    var self (default,null) :WorkerContext;

    var notifications (default,null) :NotificationCenter;

    function clearInterval (handle :Int) :Void;

    function clearTimeout (handle :Int) :Void;

    function close () :Void;

    function importScripts () :Void;

    function openDatabase (name :String, version :String, displayName :String, estimatedSize :Int, ?creationCallback :DatabaseCallback) :Database;

    function openDatabaseSync (name :String, version :String, displayName :String, estimatedSize :Int, ?creationCallback :DatabaseCallback) :DatabaseSync;

    function requestFileSystem (type :Int, size :Int, ?successCallback :FileSystemCallback, ?errorCallback :ErrorCallback) :Void;

    function requestFileSystemSync (type :Int, size :Int) :DOMFileSystemSync;

    function resolveLocalFileSystemSyncURL (url :String) :EntrySync;

    function resolveLocalFileSystemURL (url :String, successCallback :EntryCallback, ?errorCallback :ErrorCallback) :Void;

    function setInterval (handler :Void->Void, timeout :Int) :Int;

    function setTimeout (handler :Void->Void, timeout :Int) :Int;

}
