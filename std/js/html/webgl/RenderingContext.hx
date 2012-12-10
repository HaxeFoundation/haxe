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
package js.html.webgl;

@:native("WebGLRenderingContext")
extern class RenderingContext extends js.html.CanvasRenderingContext
{
    static inline var ACTIVE_ATTRIBUTES :Int = 0x8B89;

    static inline var ACTIVE_TEXTURE :Int = 0x84E0;

    static inline var ACTIVE_UNIFORMS :Int = 0x8B86;

    static inline var ALIASED_LINE_WIDTH_RANGE :Int = 0x846E;

    static inline var ALIASED_POINT_SIZE_RANGE :Int = 0x846D;

    static inline var ALPHA :Int = 0x1906;

    static inline var ALPHA_BITS :Int = 0x0D55;

    static inline var ALWAYS :Int = 0x0207;

    static inline var ARRAY_BUFFER :Int = 0x8892;

    static inline var ARRAY_BUFFER_BINDING :Int = 0x8894;

    static inline var ATTACHED_SHADERS :Int = 0x8B85;

    static inline var BACK :Int = 0x0405;

    static inline var BLEND :Int = 0x0BE2;

    static inline var BLEND_COLOR :Int = 0x8005;

    static inline var BLEND_DST_ALPHA :Int = 0x80CA;

    static inline var BLEND_DST_RGB :Int = 0x80C8;

    static inline var BLEND_EQUATION :Int = 0x8009;

    static inline var BLEND_EQUATION_ALPHA :Int = 0x883D;

    static inline var BLEND_EQUATION_RGB :Int = 0x8009;

    static inline var BLEND_SRC_ALPHA :Int = 0x80CB;

    static inline var BLEND_SRC_RGB :Int = 0x80C9;

    static inline var BLUE_BITS :Int = 0x0D54;

    static inline var BOOL :Int = 0x8B56;

    static inline var BOOL_VEC2 :Int = 0x8B57;

    static inline var BOOL_VEC3 :Int = 0x8B58;

    static inline var BOOL_VEC4 :Int = 0x8B59;

    static inline var BROWSER_DEFAULT_WEBGL :Int = 0x9244;

    static inline var BUFFER_SIZE :Int = 0x8764;

    static inline var BUFFER_USAGE :Int = 0x8765;

    static inline var BYTE :Int = 0x1400;

    static inline var CCW :Int = 0x0901;

    static inline var CLAMP_TO_EDGE :Int = 0x812F;

    static inline var COLOR_ATTACHMENT0 :Int = 0x8CE0;

    static inline var COLOR_BUFFER_BIT :Int = 0x00004000;

    static inline var COLOR_CLEAR_VALUE :Int = 0x0C22;

    static inline var COLOR_WRITEMASK :Int = 0x0C23;

    static inline var COMPILE_STATUS :Int = 0x8B81;

    static inline var COMPRESSED_TEXTURE_FORMATS :Int = 0x86A3;

    static inline var CONSTANT_ALPHA :Int = 0x8003;

    static inline var CONSTANT_COLOR :Int = 0x8001;

    static inline var CONTEXT_LOST_WEBGL :Int = 0x9242;

    static inline var CULL_FACE :Int = 0x0B44;

    static inline var CULL_FACE_MODE :Int = 0x0B45;

    static inline var CURRENT_PROGRAM :Int = 0x8B8D;

    static inline var CURRENT_VERTEX_ATTRIB :Int = 0x8626;

    static inline var CW :Int = 0x0900;

    static inline var DECR :Int = 0x1E03;

    static inline var DECR_WRAP :Int = 0x8508;

    static inline var DELETE_STATUS :Int = 0x8B80;

    static inline var DEPTH_ATTACHMENT :Int = 0x8D00;

    static inline var DEPTH_BITS :Int = 0x0D56;

    static inline var DEPTH_BUFFER_BIT :Int = 0x00000100;

    static inline var DEPTH_CLEAR_VALUE :Int = 0x0B73;

    static inline var DEPTH_COMPONENT :Int = 0x1902;

    static inline var DEPTH_COMPONENT16 :Int = 0x81A5;

    static inline var DEPTH_FUNC :Int = 0x0B74;

    static inline var DEPTH_RANGE :Int = 0x0B70;

    static inline var DEPTH_STENCIL :Int = 0x84F9;

    static inline var DEPTH_STENCIL_ATTACHMENT :Int = 0x821A;

    static inline var DEPTH_TEST :Int = 0x0B71;

    static inline var DEPTH_WRITEMASK :Int = 0x0B72;

    static inline var DITHER :Int = 0x0BD0;

    static inline var DONT_CARE :Int = 0x1100;

    static inline var DST_ALPHA :Int = 0x0304;

    static inline var DST_COLOR :Int = 0x0306;

    static inline var DYNAMIC_DRAW :Int = 0x88E8;

    static inline var ELEMENT_ARRAY_BUFFER :Int = 0x8893;

    static inline var ELEMENT_ARRAY_BUFFER_BINDING :Int = 0x8895;

    static inline var EQUAL :Int = 0x0202;

    static inline var FASTEST :Int = 0x1101;

    static inline var FLOAT :Int = 0x1406;

    static inline var FLOAT_MAT2 :Int = 0x8B5A;

    static inline var FLOAT_MAT3 :Int = 0x8B5B;

    static inline var FLOAT_MAT4 :Int = 0x8B5C;

    static inline var FLOAT_VEC2 :Int = 0x8B50;

    static inline var FLOAT_VEC3 :Int = 0x8B51;

    static inline var FLOAT_VEC4 :Int = 0x8B52;

    static inline var FRAGMENT_SHADER :Int = 0x8B30;

    static inline var FRAMEBUFFER :Int = 0x8D40;

    static inline var FRAMEBUFFER_ATTACHMENT_OBJECT_NAME :Int = 0x8CD1;

    static inline var FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE :Int = 0x8CD0;

    static inline var FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE :Int = 0x8CD3;

    static inline var FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL :Int = 0x8CD2;

    static inline var FRAMEBUFFER_BINDING :Int = 0x8CA6;

    static inline var FRAMEBUFFER_COMPLETE :Int = 0x8CD5;

    static inline var FRAMEBUFFER_INCOMPLETE_ATTACHMENT :Int = 0x8CD6;

    static inline var FRAMEBUFFER_INCOMPLETE_DIMENSIONS :Int = 0x8CD9;

    static inline var FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT :Int = 0x8CD7;

    static inline var FRAMEBUFFER_UNSUPPORTED :Int = 0x8CDD;

    static inline var FRONT :Int = 0x0404;

    static inline var FRONT_AND_BACK :Int = 0x0408;

    static inline var FRONT_FACE :Int = 0x0B46;

    static inline var FUNC_ADD :Int = 0x8006;

    static inline var FUNC_REVERSE_SUBTRACT :Int = 0x800B;

    static inline var FUNC_SUBTRACT :Int = 0x800A;

    static inline var GENERATE_MIPMAP_HINT :Int = 0x8192;

    static inline var GEQUAL :Int = 0x0206;

    static inline var GREATER :Int = 0x0204;

    static inline var GREEN_BITS :Int = 0x0D53;

    static inline var HIGH_FLOAT :Int = 0x8DF2;

    static inline var HIGH_INT :Int = 0x8DF5;

    static inline var INCR :Int = 0x1E02;

    static inline var INCR_WRAP :Int = 0x8507;

    static inline var INT :Int = 0x1404;

    static inline var INT_VEC2 :Int = 0x8B53;

    static inline var INT_VEC3 :Int = 0x8B54;

    static inline var INT_VEC4 :Int = 0x8B55;

    static inline var INVALID_ENUM :Int = 0x0500;

    static inline var INVALID_FRAMEBUFFER_OPERATION :Int = 0x0506;

    static inline var INVALID_OPERATION :Int = 0x0502;

    static inline var INVALID_VALUE :Int = 0x0501;

    static inline var INVERT :Int = 0x150A;

    static inline var KEEP :Int = 0x1E00;

    static inline var LEQUAL :Int = 0x0203;

    static inline var LESS :Int = 0x0201;

    static inline var LINEAR :Int = 0x2601;

    static inline var LINEAR_MIPMAP_LINEAR :Int = 0x2703;

    static inline var LINEAR_MIPMAP_NEAREST :Int = 0x2701;

    static inline var LINES :Int = 0x0001;

    static inline var LINE_LOOP :Int = 0x0002;

    static inline var LINE_STRIP :Int = 0x0003;

    static inline var LINE_WIDTH :Int = 0x0B21;

    static inline var LINK_STATUS :Int = 0x8B82;

    static inline var LOW_FLOAT :Int = 0x8DF0;

    static inline var LOW_INT :Int = 0x8DF3;

    static inline var LUMINANCE :Int = 0x1909;

    static inline var LUMINANCE_ALPHA :Int = 0x190A;

    static inline var MAX_COMBINED_TEXTURE_IMAGE_UNITS :Int = 0x8B4D;

    static inline var MAX_CUBE_MAP_TEXTURE_SIZE :Int = 0x851C;

    static inline var MAX_FRAGMENT_UNIFORM_VECTORS :Int = 0x8DFD;

    static inline var MAX_RENDERBUFFER_SIZE :Int = 0x84E8;

    static inline var MAX_TEXTURE_IMAGE_UNITS :Int = 0x8872;

    static inline var MAX_TEXTURE_SIZE :Int = 0x0D33;

    static inline var MAX_VARYING_VECTORS :Int = 0x8DFC;

    static inline var MAX_VERTEX_ATTRIBS :Int = 0x8869;

    static inline var MAX_VERTEX_TEXTURE_IMAGE_UNITS :Int = 0x8B4C;

    static inline var MAX_VERTEX_UNIFORM_VECTORS :Int = 0x8DFB;

    static inline var MAX_VIEWPORT_DIMS :Int = 0x0D3A;

    static inline var MEDIUM_FLOAT :Int = 0x8DF1;

    static inline var MEDIUM_INT :Int = 0x8DF4;

    static inline var MIRRORED_REPEAT :Int = 0x8370;

    static inline var NEAREST :Int = 0x2600;

    static inline var NEAREST_MIPMAP_LINEAR :Int = 0x2702;

    static inline var NEAREST_MIPMAP_NEAREST :Int = 0x2700;

    static inline var NEVER :Int = 0x0200;

    static inline var NICEST :Int = 0x1102;

    static inline var NONE :Int = 0;

    static inline var NOTEQUAL :Int = 0x0205;

    static inline var NO_ERROR :Int = 0;

    static inline var ONE :Int = 1;

    static inline var ONE_MINUS_CONSTANT_ALPHA :Int = 0x8004;

    static inline var ONE_MINUS_CONSTANT_COLOR :Int = 0x8002;

    static inline var ONE_MINUS_DST_ALPHA :Int = 0x0305;

    static inline var ONE_MINUS_DST_COLOR :Int = 0x0307;

    static inline var ONE_MINUS_SRC_ALPHA :Int = 0x0303;

    static inline var ONE_MINUS_SRC_COLOR :Int = 0x0301;

    static inline var OUT_OF_MEMORY :Int = 0x0505;

    static inline var PACK_ALIGNMENT :Int = 0x0D05;

    static inline var POINTS :Int = 0x0000;

    static inline var POLYGON_OFFSET_FACTOR :Int = 0x8038;

    static inline var POLYGON_OFFSET_FILL :Int = 0x8037;

    static inline var POLYGON_OFFSET_UNITS :Int = 0x2A00;

    static inline var RED_BITS :Int = 0x0D52;

    static inline var RENDERBUFFER :Int = 0x8D41;

    static inline var RENDERBUFFER_ALPHA_SIZE :Int = 0x8D53;

    static inline var RENDERBUFFER_BINDING :Int = 0x8CA7;

    static inline var RENDERBUFFER_BLUE_SIZE :Int = 0x8D52;

    static inline var RENDERBUFFER_DEPTH_SIZE :Int = 0x8D54;

    static inline var RENDERBUFFER_GREEN_SIZE :Int = 0x8D51;

    static inline var RENDERBUFFER_HEIGHT :Int = 0x8D43;

    static inline var RENDERBUFFER_INTERNAL_FORMAT :Int = 0x8D44;

    static inline var RENDERBUFFER_RED_SIZE :Int = 0x8D50;

    static inline var RENDERBUFFER_STENCIL_SIZE :Int = 0x8D55;

    static inline var RENDERBUFFER_WIDTH :Int = 0x8D42;

    static inline var RENDERER :Int = 0x1F01;

    static inline var REPEAT :Int = 0x2901;

    static inline var REPLACE :Int = 0x1E01;

    static inline var RGB :Int = 0x1907;

    static inline var RGB565 :Int = 0x8D62;

    static inline var RGB5_A1 :Int = 0x8057;

    static inline var RGBA :Int = 0x1908;

    static inline var RGBA4 :Int = 0x8056;

    static inline var SAMPLER_2D :Int = 0x8B5E;

    static inline var SAMPLER_CUBE :Int = 0x8B60;

    static inline var SAMPLES :Int = 0x80A9;

    static inline var SAMPLE_ALPHA_TO_COVERAGE :Int = 0x809E;

    static inline var SAMPLE_BUFFERS :Int = 0x80A8;

    static inline var SAMPLE_COVERAGE :Int = 0x80A0;

    static inline var SAMPLE_COVERAGE_INVERT :Int = 0x80AB;

    static inline var SAMPLE_COVERAGE_VALUE :Int = 0x80AA;

    static inline var SCISSOR_BOX :Int = 0x0C10;

    static inline var SCISSOR_TEST :Int = 0x0C11;

    static inline var SHADER_TYPE :Int = 0x8B4F;

    static inline var SHADING_LANGUAGE_VERSION :Int = 0x8B8C;

    static inline var SHORT :Int = 0x1402;

    static inline var SRC_ALPHA :Int = 0x0302;

    static inline var SRC_ALPHA_SATURATE :Int = 0x0308;

    static inline var SRC_COLOR :Int = 0x0300;

    static inline var STATIC_DRAW :Int = 0x88E4;

    static inline var STENCIL_ATTACHMENT :Int = 0x8D20;

    static inline var STENCIL_BACK_FAIL :Int = 0x8801;

    static inline var STENCIL_BACK_FUNC :Int = 0x8800;

    static inline var STENCIL_BACK_PASS_DEPTH_FAIL :Int = 0x8802;

    static inline var STENCIL_BACK_PASS_DEPTH_PASS :Int = 0x8803;

    static inline var STENCIL_BACK_REF :Int = 0x8CA3;

    static inline var STENCIL_BACK_VALUE_MASK :Int = 0x8CA4;

    static inline var STENCIL_BACK_WRITEMASK :Int = 0x8CA5;

    static inline var STENCIL_BITS :Int = 0x0D57;

    static inline var STENCIL_BUFFER_BIT :Int = 0x00000400;

    static inline var STENCIL_CLEAR_VALUE :Int = 0x0B91;

    static inline var STENCIL_FAIL :Int = 0x0B94;

    static inline var STENCIL_FUNC :Int = 0x0B92;

    static inline var STENCIL_INDEX :Int = 0x1901;

    static inline var STENCIL_INDEX8 :Int = 0x8D48;

    static inline var STENCIL_PASS_DEPTH_FAIL :Int = 0x0B95;

    static inline var STENCIL_PASS_DEPTH_PASS :Int = 0x0B96;

    static inline var STENCIL_REF :Int = 0x0B97;

    static inline var STENCIL_TEST :Int = 0x0B90;

    static inline var STENCIL_VALUE_MASK :Int = 0x0B93;

    static inline var STENCIL_WRITEMASK :Int = 0x0B98;

    static inline var STREAM_DRAW :Int = 0x88E0;

    static inline var SUBPIXEL_BITS :Int = 0x0D50;

    static inline var TEXTURE :Int = 0x1702;

    static inline var TEXTURE0 :Int = 0x84C0;

    static inline var TEXTURE1 :Int = 0x84C1;

    static inline var TEXTURE10 :Int = 0x84CA;

    static inline var TEXTURE11 :Int = 0x84CB;

    static inline var TEXTURE12 :Int = 0x84CC;

    static inline var TEXTURE13 :Int = 0x84CD;

    static inline var TEXTURE14 :Int = 0x84CE;

    static inline var TEXTURE15 :Int = 0x84CF;

    static inline var TEXTURE16 :Int = 0x84D0;

    static inline var TEXTURE17 :Int = 0x84D1;

    static inline var TEXTURE18 :Int = 0x84D2;

    static inline var TEXTURE19 :Int = 0x84D3;

    static inline var TEXTURE2 :Int = 0x84C2;

    static inline var TEXTURE20 :Int = 0x84D4;

    static inline var TEXTURE21 :Int = 0x84D5;

    static inline var TEXTURE22 :Int = 0x84D6;

    static inline var TEXTURE23 :Int = 0x84D7;

    static inline var TEXTURE24 :Int = 0x84D8;

    static inline var TEXTURE25 :Int = 0x84D9;

    static inline var TEXTURE26 :Int = 0x84DA;

    static inline var TEXTURE27 :Int = 0x84DB;

    static inline var TEXTURE28 :Int = 0x84DC;

    static inline var TEXTURE29 :Int = 0x84DD;

    static inline var TEXTURE3 :Int = 0x84C3;

    static inline var TEXTURE30 :Int = 0x84DE;

    static inline var TEXTURE31 :Int = 0x84DF;

    static inline var TEXTURE4 :Int = 0x84C4;

    static inline var TEXTURE5 :Int = 0x84C5;

    static inline var TEXTURE6 :Int = 0x84C6;

    static inline var TEXTURE7 :Int = 0x84C7;

    static inline var TEXTURE8 :Int = 0x84C8;

    static inline var TEXTURE9 :Int = 0x84C9;

    static inline var TEXTURE_2D :Int = 0x0DE1;

    static inline var TEXTURE_BINDING_2D :Int = 0x8069;

    static inline var TEXTURE_BINDING_CUBE_MAP :Int = 0x8514;

    static inline var TEXTURE_CUBE_MAP :Int = 0x8513;

    static inline var TEXTURE_CUBE_MAP_NEGATIVE_X :Int = 0x8516;

    static inline var TEXTURE_CUBE_MAP_NEGATIVE_Y :Int = 0x8518;

    static inline var TEXTURE_CUBE_MAP_NEGATIVE_Z :Int = 0x851A;

    static inline var TEXTURE_CUBE_MAP_POSITIVE_X :Int = 0x8515;

    static inline var TEXTURE_CUBE_MAP_POSITIVE_Y :Int = 0x8517;

    static inline var TEXTURE_CUBE_MAP_POSITIVE_Z :Int = 0x8519;

    static inline var TEXTURE_MAG_FILTER :Int = 0x2800;

    static inline var TEXTURE_MIN_FILTER :Int = 0x2801;

    static inline var TEXTURE_WRAP_S :Int = 0x2802;

    static inline var TEXTURE_WRAP_T :Int = 0x2803;

    static inline var TRIANGLES :Int = 0x0004;

    static inline var TRIANGLE_FAN :Int = 0x0006;

    static inline var TRIANGLE_STRIP :Int = 0x0005;

    static inline var UNPACK_ALIGNMENT :Int = 0x0CF5;

    static inline var UNPACK_COLORSPACE_CONVERSION_WEBGL :Int = 0x9243;

    static inline var UNPACK_FLIP_Y_WEBGL :Int = 0x9240;

    static inline var UNPACK_PREMULTIPLY_ALPHA_WEBGL :Int = 0x9241;

    static inline var UNSIGNED_BYTE :Int = 0x1401;

    static inline var UNSIGNED_INT :Int = 0x1405;

    static inline var UNSIGNED_SHORT :Int = 0x1403;

    static inline var UNSIGNED_SHORT_4_4_4_4 :Int = 0x8033;

    static inline var UNSIGNED_SHORT_5_5_5_1 :Int = 0x8034;

    static inline var UNSIGNED_SHORT_5_6_5 :Int = 0x8363;

    static inline var VALIDATE_STATUS :Int = 0x8B83;

    static inline var VENDOR :Int = 0x1F00;

    static inline var VERSION :Int = 0x1F02;

    static inline var VERTEX_ATTRIB_ARRAY_BUFFER_BINDING :Int = 0x889F;

    static inline var VERTEX_ATTRIB_ARRAY_ENABLED :Int = 0x8622;

    static inline var VERTEX_ATTRIB_ARRAY_NORMALIZED :Int = 0x886A;

    static inline var VERTEX_ATTRIB_ARRAY_POINTER :Int = 0x8645;

    static inline var VERTEX_ATTRIB_ARRAY_SIZE :Int = 0x8623;

    static inline var VERTEX_ATTRIB_ARRAY_STRIDE :Int = 0x8624;

    static inline var VERTEX_ATTRIB_ARRAY_TYPE :Int = 0x8625;

    static inline var VERTEX_SHADER :Int = 0x8B31;

    static inline var VIEWPORT :Int = 0x0BA2;

    static inline var ZERO :Int = 0;

    var drawingBufferHeight (default,null) :Int;

    var drawingBufferWidth (default,null) :Int;

    function activeTexture (texture :Int) :Void;

    function attachShader (program :Program, shader :Shader) :Void;

    function bindAttribLocation (program :Program, index :Int, name :String) :Void;

    function bindBuffer (target :Int, buffer :Buffer) :Void;

    function bindFramebuffer (target :Int, framebuffer :Framebuffer) :Void;

    function bindRenderbuffer (target :Int, renderbuffer :Renderbuffer) :Void;

    function bindTexture (target :Int, texture :Texture) :Void;

    function blendColor (red :Float, green :Float, blue :Float, alpha :Float) :Void;

    function blendEquation (mode :Int) :Void;

    function blendEquationSeparate (modeRGB :Int, modeAlpha :Int) :Void;

    function blendFunc (sfactor :Int, dfactor :Int) :Void;

    function blendFuncSeparate (srcRGB :Int, dstRGB :Int, srcAlpha :Int, dstAlpha :Int) :Void;

    /** Throws DOMException. */
    @:overload(function (target :Int, data :js.html.ArrayBuffer, usage :Int) :Void {})
    @:overload(function (target :Int, data :js.html.ArrayBufferView, usage :Int) :Void {})
    function bufferData (target :Int, size :Int, usage :Int) :Void;

    /** Throws DOMException. */
    @:overload(function (target :Int, offset :Int, data :js.html.ArrayBuffer) :Void {})
    function bufferSubData (target :Int, offset :Int, data :js.html.ArrayBufferView) :Void;

    function checkFramebufferStatus (target :Int) :Int;

    function clear (mask :Int) :Void;

    function clearColor (red :Float, green :Float, blue :Float, alpha :Float) :Void;

    function clearDepth (depth :Float) :Void;

    function clearStencil (s :Int) :Void;

    function colorMask (red :Bool, green :Bool, blue :Bool, alpha :Bool) :Void;

    function compileShader (shader :Shader) :Void;

    function compressedTexImage2D (target :Int, level :Int, internalformat :Int, width :Int, height :Int, border :Int, data :js.html.ArrayBufferView) :Void;

    function compressedTexSubImage2D (target :Int, level :Int, xoffset :Int, yoffset :Int, width :Int, height :Int, format :Int, data :js.html.ArrayBufferView) :Void;

    function copyTexImage2D (target :Int, level :Int, internalformat :Int, x :Int, y :Int, width :Int, height :Int, border :Int) :Void;

    function copyTexSubImage2D (target :Int, level :Int, xoffset :Int, yoffset :Int, x :Int, y :Int, width :Int, height :Int) :Void;

    function createBuffer () :Buffer;

    function createFramebuffer () :Framebuffer;

    function createProgram () :Program;

    function createRenderbuffer () :Renderbuffer;

    function createShader (type :Int) :Shader;

    function createTexture () :Texture;

    function cullFace (mode :Int) :Void;

    function deleteBuffer (buffer :Buffer) :Void;

    function deleteFramebuffer (framebuffer :Framebuffer) :Void;

    function deleteProgram (program :Program) :Void;

    function deleteRenderbuffer (renderbuffer :Renderbuffer) :Void;

    function deleteShader (shader :Shader) :Void;

    function deleteTexture (texture :Texture) :Void;

    function depthFunc (func :Int) :Void;

    function depthMask (flag :Bool) :Void;

    function depthRange (zNear :Float, zFar :Float) :Void;

    function detachShader (program :Program, shader :Shader) :Void;

    function disable (cap :Int) :Void;

    function disableVertexAttribArray (index :Int) :Void;

    function drawArrays (mode :Int, first :Int, count :Int) :Void;

    function drawElements (mode :Int, count :Int, type :Int, offset :Int) :Void;

    function enable (cap :Int) :Void;

    function enableVertexAttribArray (index :Int) :Void;

    function finish () :Void;

    function flush () :Void;

    function framebufferRenderbuffer (target :Int, attachment :Int, renderbuffertarget :Int, renderbuffer :Renderbuffer) :Void;

    function framebufferTexture2D (target :Int, attachment :Int, textarget :Int, texture :Texture, level :Int) :Void;

    function frontFace (mode :Int) :Void;

    function generateMipmap (target :Int) :Void;

    function getActiveAttrib (program :Program, index :Int) :ActiveInfo;

    function getActiveUniform (program :Program, index :Int) :ActiveInfo;

    function getAttachedShaders (program :Program) :Void;

    function getAttribLocation (program :Program, name :String) :Int;

    function getBufferParameter () :Void;

    function getContextAttributes () :ContextAttributes;

    function getError () :Int;

    function getExtension (name :String) :Dynamic;

    function getFramebufferAttachmentParameter () :Void;

    function getParameter () :Void;

    function getProgramInfoLog (program :Program) :String;

    function getProgramParameter () :Void;

    function getRenderbufferParameter () :Void;

    function getShaderInfoLog (shader :Shader) :String;

    function getShaderParameter () :Void;

    function getShaderPrecisionFormat (shadertype :Int, precisiontype :Int) :ShaderPrecisionFormat;

    function getShaderSource (shader :Shader) :String;

    function getSupportedExtensions () :Array<String>;

    function getTexParameter () :Void;

    function getUniform () :Void;

    function getUniformLocation (program :Program, name :String) :UniformLocation;

    function getVertexAttrib () :Void;

    function getVertexAttribOffset (index :Int, pname :Int) :Int;

    function hint (target :Int, mode :Int) :Void;

    function isBuffer (buffer :Buffer) :Bool;

    function isContextLost () :Bool;

    function isEnabled (cap :Int) :Bool;

    function isFramebuffer (framebuffer :Framebuffer) :Bool;

    function isProgram (program :Program) :Bool;

    function isRenderbuffer (renderbuffer :Renderbuffer) :Bool;

    function isShader (shader :Shader) :Bool;

    function isTexture (texture :Texture) :Bool;

    function lineWidth (width :Float) :Void;

    function linkProgram (program :Program) :Void;

    function pixelStorei (pname :Int, param :Int) :Void;

    function polygonOffset (factor :Float, units :Float) :Void;

    function readPixels (x :Int, y :Int, width :Int, height :Int, format :Int, type :Int, pixels :js.html.ArrayBufferView) :Void;

    function releaseShaderCompiler () :Void;

    function renderbufferStorage (target :Int, internalformat :Int, width :Int, height :Int) :Void;

    function sampleCoverage (value :Float, invert :Bool) :Void;

    function scissor (x :Int, y :Int, width :Int, height :Int) :Void;

    function shaderSource (shader :Shader, string :String) :Void;

    function stencilFunc (func :Int, ref :Int, mask :Int) :Void;

    function stencilFuncSeparate (face :Int, func :Int, ref :Int, mask :Int) :Void;

    function stencilMask (mask :Int) :Void;

    function stencilMaskSeparate (face :Int, mask :Int) :Void;

    function stencilOp (fail :Int, zfail :Int, zpass :Int) :Void;

    function stencilOpSeparate (face :Int, fail :Int, zfail :Int, zpass :Int) :Void;

    /** Throws DOMException. */
    @:overload(function (target :Int, level :Int, internalformat :Int, width :Int, height :Int, border :Int, format :Int, type :Int, pixels :js.html.ArrayBufferView) :Void {})
    @:overload(function (target :Int, level :Int, internalformat :Int, format :Int, type :Int, pixels :js.html.ImageData) :Void {})
    @:overload(function (target :Int, level :Int, internalformat :Int, format :Int, type :Int, image :js.html.ImageElement) :Void {})
    @:overload(function (target :Int, level :Int, internalformat :Int, format :Int, type :Int, canvas :js.html.CanvasElement) :Void {})
    function texImage2D (target :Int, level :Int, internalformat :Int, format :Int, type :Int, video :js.html.VideoElement) :Void;

    function texParameterf (target :Int, pname :Int, param :Float) :Void;

    function texParameteri (target :Int, pname :Int, param :Int) :Void;

    /** Throws DOMException. */
    @:overload(function (target :Int, level :Int, xoffset :Int, yoffset :Int, width :Int, height :Int, format :Int, type :Int, pixels :js.html.ArrayBufferView) :Void {})
    @:overload(function (target :Int, level :Int, xoffset :Int, yoffset :Int, format :Int, type :Int, pixels :js.html.ImageData) :Void {})
    @:overload(function (target :Int, level :Int, xoffset :Int, yoffset :Int, format :Int, type :Int, image :js.html.ImageElement) :Void {})
    @:overload(function (target :Int, level :Int, xoffset :Int, yoffset :Int, format :Int, type :Int, canvas :js.html.CanvasElement) :Void {})
    function texSubImage2D (target :Int, level :Int, xoffset :Int, yoffset :Int, format :Int, type :Int, video :js.html.VideoElement) :Void;

    function uniform1f (location :UniformLocation, x :Float) :Void;

    function uniform1fv (location :UniformLocation, v :js.html.Float32Array) :Void;

    function uniform1i (location :UniformLocation, x :Int) :Void;

    function uniform1iv (location :UniformLocation, v :js.html.Int32Array) :Void;

    function uniform2f (location :UniformLocation, x :Float, y :Float) :Void;

    function uniform2fv (location :UniformLocation, v :js.html.Float32Array) :Void;

    function uniform2i (location :UniformLocation, x :Int, y :Int) :Void;

    function uniform2iv (location :UniformLocation, v :js.html.Int32Array) :Void;

    function uniform3f (location :UniformLocation, x :Float, y :Float, z :Float) :Void;

    function uniform3fv (location :UniformLocation, v :js.html.Float32Array) :Void;

    function uniform3i (location :UniformLocation, x :Int, y :Int, z :Int) :Void;

    function uniform3iv (location :UniformLocation, v :js.html.Int32Array) :Void;

    function uniform4f (location :UniformLocation, x :Float, y :Float, z :Float, w :Float) :Void;

    function uniform4fv (location :UniformLocation, v :js.html.Float32Array) :Void;

    function uniform4i (location :UniformLocation, x :Int, y :Int, z :Int, w :Int) :Void;

    function uniform4iv (location :UniformLocation, v :js.html.Int32Array) :Void;

    function uniformMatrix2fv (location :UniformLocation, transpose :Bool, array :js.html.Float32Array) :Void;

    function uniformMatrix3fv (location :UniformLocation, transpose :Bool, array :js.html.Float32Array) :Void;

    function uniformMatrix4fv (location :UniformLocation, transpose :Bool, array :js.html.Float32Array) :Void;

    function useProgram (program :Program) :Void;

    function validateProgram (program :Program) :Void;

    function vertexAttrib1f (indx :Int, x :Float) :Void;

    function vertexAttrib1fv (indx :Int, values :js.html.Float32Array) :Void;

    function vertexAttrib2f (indx :Int, x :Float, y :Float) :Void;

    function vertexAttrib2fv (indx :Int, values :js.html.Float32Array) :Void;

    function vertexAttrib3f (indx :Int, x :Float, y :Float, z :Float) :Void;

    function vertexAttrib3fv (indx :Int, values :js.html.Float32Array) :Void;

    function vertexAttrib4f (indx :Int, x :Float, y :Float, z :Float, w :Float) :Void;

    function vertexAttrib4fv (indx :Int, values :js.html.Float32Array) :Void;

    function vertexAttribPointer (indx :Int, size :Int, type :Int, normalized :Bool, stride :Int, offset :Int) :Void;

    function viewport (x :Int, y :Int, width :Int, height :Int) :Void;

}
