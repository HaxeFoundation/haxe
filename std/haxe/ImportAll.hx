/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package haxe;

// std
import Array;
import Date;
import DateTools;
import EReg;
import Hash;
import IntIter;
import Iterator;
import Lambda;
import List;
import Math;
import Md5;
import Node;
import Reflect;
import Std;
import StdTypes;
import String;
import StringBuf;
import StringTools;
import XmlParser;

import haxe.Connection;
import haxe.ImportAll;
import haxe.Log;
import haxe.PosInfos;
import haxe.Serializer;
import haxe.Template;
import haxe.Unserializer;

// flash
#if flash

import flash.Boot;
import flash.Lib;

import flash.Accessibility;
import flash.Camera;
import flash.Color;
import flash.Key;
import flash.LoadVars;
import flash.LocalConnection;
import flash.Microphone;
import flash.Mouse;
import flash.MovieClip;
import flash.MovieClipLoader;
import flash.NetConnection;
import flash.NetStream;
import flash.PrintJob;
import flash.Selection;
import flash.SharedObject;
import flash.Sound;
import flash.Stage;
import flash.System;
import flash.TextField;
import flash.TextFormat;
import flash.TextSnapshot;
import flash.Timer;
import flash.Video;
import flash.XMLSocket;

import flash.text.StyleSheet;
import flash.system.Capabilities;
import flash.system.Security;

#end

#if flash8

import flash.display.BitmapData;
import flash.external.ExternalInterface;
import flash.filters.BevelFilter;
import flash.filters.BitmapFilter;
import flash.filters.BlurFilter;
import flash.filters.ColorMatrixFilter;
import flash.filters.ConvolutionFilter;
import flash.filters.DisplacementMapFilter;
import flash.filters.DropShadowFilter;
import flash.filters.GlowFilter;
import flash.filters.GradientBevelFilter;
import flash.filters.GradientGlowFilter;

import flash.geom.ColorTransform;
import flash.geom.Matrix;
import flash.geom.Point;
import flash.geom.Rectangle;
import flash.geom.Transform;

import flash.net.FileReference;
import flash.net.FileReferenceList;

import flash.system.IME;
import flash.text.TextRenderer;

#end

#if neko

import neko.Boot;
import neko.File;
import neko.FileSystem;
import neko.Lib;
import neko.Socket;
import neko.Stack;
import neko.Sys;
import neko.Web;

import neko.db.Mysql;
import neko.db.Connection;
import neko.db.ResultSet;
import neko.db.Object;
import neko.db.Manager;
import neko.db.Transaction;

import tools.DocView;

#end

#if js

import js.Boot;
import js.Lib;

import js.Anchor;
import js.Body;
import js.Button;
import js.Checkbox;
import js.Document;
import js.Event;
import js.FileUpload;
import js.Form;
import js.FormElement;
import js.Frame;
import js.FrameSet;
import js.Hidden;
import js.History;
import js.HtmlCollection;
import js.HtmlDom;
import js.IFrame;
import js.Image;
import js.Link;
import js.Location;
import js.Navigator;
import js.Option;
import js.Password;
import js.Radio;
import js.Reset;
import js.Screen;
import js.Select;
import js.Style;
import js.StyleSheet;
import js.Submit;
import js.Text;
import js.Textarea;
import js.Window;
import js.XMLHttpRequest;

#end
