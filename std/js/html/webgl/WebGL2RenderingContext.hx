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

// This file is generated from mozilla\WebGL2RenderingContext.webidl. Do not edit!

package js.html.webgl;

/**
	The WebGL2RenderingContext interface provides the OpenGL ES 3.0 rendering context for the drawing surface of an HTML `canvas` element.

	Documentation [WebGL2RenderingContext](https://developer.mozilla.org/en-US/docs/Web/API/WebGL2RenderingContext) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/WebGL2RenderingContext$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/WebGL2RenderingContext>
**/
@:native("WebGL2RenderingContext")
extern class WebGL2RenderingContext extends RenderingContext {
	static inline var READ_BUFFER : Int = 3074;
	static inline var UNPACK_ROW_LENGTH : Int = 3314;
	static inline var UNPACK_SKIP_ROWS : Int = 3315;
	static inline var UNPACK_SKIP_PIXELS : Int = 3316;
	static inline var PACK_ROW_LENGTH : Int = 3330;
	static inline var PACK_SKIP_ROWS : Int = 3331;
	static inline var PACK_SKIP_PIXELS : Int = 3332;
	static inline var COLOR : Int = 6144;
	static inline var DEPTH : Int = 6145;
	static inline var STENCIL : Int = 6146;
	static inline var RED : Int = 6403;
	static inline var RGB8 : Int = 32849;
	static inline var RGBA8 : Int = 32856;
	static inline var RGB10_A2 : Int = 32857;
	static inline var TEXTURE_BINDING_3D : Int = 32874;
	static inline var UNPACK_SKIP_IMAGES : Int = 32877;
	static inline var UNPACK_IMAGE_HEIGHT : Int = 32878;
	static inline var TEXTURE_3D : Int = 32879;
	static inline var TEXTURE_WRAP_R : Int = 32882;
	static inline var MAX_3D_TEXTURE_SIZE : Int = 32883;
	static inline var UNSIGNED_INT_2_10_10_10_REV : Int = 33640;
	static inline var MAX_ELEMENTS_VERTICES : Int = 33000;
	static inline var MAX_ELEMENTS_INDICES : Int = 33001;
	static inline var TEXTURE_MIN_LOD : Int = 33082;
	static inline var TEXTURE_MAX_LOD : Int = 33083;
	static inline var TEXTURE_BASE_LEVEL : Int = 33084;
	static inline var TEXTURE_MAX_LEVEL : Int = 33085;
	static inline var MIN : Int = 32775;
	static inline var MAX : Int = 32776;
	static inline var DEPTH_COMPONENT24 : Int = 33190;
	static inline var MAX_TEXTURE_LOD_BIAS : Int = 34045;
	static inline var TEXTURE_COMPARE_MODE : Int = 34892;
	static inline var TEXTURE_COMPARE_FUNC : Int = 34893;
	static inline var CURRENT_QUERY : Int = 34917;
	static inline var QUERY_RESULT : Int = 34918;
	static inline var QUERY_RESULT_AVAILABLE : Int = 34919;
	static inline var STREAM_READ : Int = 35041;
	static inline var STREAM_COPY : Int = 35042;
	static inline var STATIC_READ : Int = 35045;
	static inline var STATIC_COPY : Int = 35046;
	static inline var DYNAMIC_READ : Int = 35049;
	static inline var DYNAMIC_COPY : Int = 35050;
	static inline var MAX_DRAW_BUFFERS : Int = 34852;
	static inline var DRAW_BUFFER0 : Int = 34853;
	static inline var DRAW_BUFFER1 : Int = 34854;
	static inline var DRAW_BUFFER2 : Int = 34855;
	static inline var DRAW_BUFFER3 : Int = 34856;
	static inline var DRAW_BUFFER4 : Int = 34857;
	static inline var DRAW_BUFFER5 : Int = 34858;
	static inline var DRAW_BUFFER6 : Int = 34859;
	static inline var DRAW_BUFFER7 : Int = 34860;
	static inline var DRAW_BUFFER8 : Int = 34861;
	static inline var DRAW_BUFFER9 : Int = 34862;
	static inline var DRAW_BUFFER10 : Int = 34863;
	static inline var DRAW_BUFFER11 : Int = 34864;
	static inline var DRAW_BUFFER12 : Int = 34865;
	static inline var DRAW_BUFFER13 : Int = 34866;
	static inline var DRAW_BUFFER14 : Int = 34867;
	static inline var DRAW_BUFFER15 : Int = 34868;
	static inline var MAX_FRAGMENT_UNIFORM_COMPONENTS : Int = 35657;
	static inline var MAX_VERTEX_UNIFORM_COMPONENTS : Int = 35658;
	static inline var SAMPLER_3D : Int = 35679;
	static inline var SAMPLER_2D_SHADOW : Int = 35682;
	static inline var FRAGMENT_SHADER_DERIVATIVE_HINT : Int = 35723;
	static inline var PIXEL_PACK_BUFFER : Int = 35051;
	static inline var PIXEL_UNPACK_BUFFER : Int = 35052;
	static inline var PIXEL_PACK_BUFFER_BINDING : Int = 35053;
	static inline var PIXEL_UNPACK_BUFFER_BINDING : Int = 35055;
	static inline var FLOAT_MAT2x3 : Int = 35685;
	static inline var FLOAT_MAT2x4 : Int = 35686;
	static inline var FLOAT_MAT3x2 : Int = 35687;
	static inline var FLOAT_MAT3x4 : Int = 35688;
	static inline var FLOAT_MAT4x2 : Int = 35689;
	static inline var FLOAT_MAT4x3 : Int = 35690;
	static inline var SRGB : Int = 35904;
	static inline var SRGB8 : Int = 35905;
	static inline var SRGB8_ALPHA8 : Int = 35907;
	static inline var COMPARE_REF_TO_TEXTURE : Int = 34894;
	static inline var RGBA32F : Int = 34836;
	static inline var RGB32F : Int = 34837;
	static inline var RGBA16F : Int = 34842;
	static inline var RGB16F : Int = 34843;
	static inline var VERTEX_ATTRIB_ARRAY_INTEGER : Int = 35069;
	static inline var MAX_ARRAY_TEXTURE_LAYERS : Int = 35071;
	static inline var MIN_PROGRAM_TEXEL_OFFSET : Int = 35076;
	static inline var MAX_PROGRAM_TEXEL_OFFSET : Int = 35077;
	static inline var MAX_VARYING_COMPONENTS : Int = 35659;
	static inline var TEXTURE_2D_ARRAY : Int = 35866;
	static inline var TEXTURE_BINDING_2D_ARRAY : Int = 35869;
	static inline var R11F_G11F_B10F : Int = 35898;
	static inline var UNSIGNED_INT_10F_11F_11F_REV : Int = 35899;
	static inline var RGB9_E5 : Int = 35901;
	static inline var UNSIGNED_INT_5_9_9_9_REV : Int = 35902;
	static inline var TRANSFORM_FEEDBACK_BUFFER_MODE : Int = 35967;
	static inline var MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS : Int = 35968;
	static inline var TRANSFORM_FEEDBACK_VARYINGS : Int = 35971;
	static inline var TRANSFORM_FEEDBACK_BUFFER_START : Int = 35972;
	static inline var TRANSFORM_FEEDBACK_BUFFER_SIZE : Int = 35973;
	static inline var TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN : Int = 35976;
	static inline var RASTERIZER_DISCARD : Int = 35977;
	static inline var MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS : Int = 35978;
	static inline var MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS : Int = 35979;
	static inline var INTERLEAVED_ATTRIBS : Int = 35980;
	static inline var SEPARATE_ATTRIBS : Int = 35981;
	static inline var TRANSFORM_FEEDBACK_BUFFER : Int = 35982;
	static inline var TRANSFORM_FEEDBACK_BUFFER_BINDING : Int = 35983;
	static inline var RGBA32UI : Int = 36208;
	static inline var RGB32UI : Int = 36209;
	static inline var RGBA16UI : Int = 36214;
	static inline var RGB16UI : Int = 36215;
	static inline var RGBA8UI : Int = 36220;
	static inline var RGB8UI : Int = 36221;
	static inline var RGBA32I : Int = 36226;
	static inline var RGB32I : Int = 36227;
	static inline var RGBA16I : Int = 36232;
	static inline var RGB16I : Int = 36233;
	static inline var RGBA8I : Int = 36238;
	static inline var RGB8I : Int = 36239;
	static inline var RED_INTEGER : Int = 36244;
	static inline var RGB_INTEGER : Int = 36248;
	static inline var RGBA_INTEGER : Int = 36249;
	static inline var SAMPLER_2D_ARRAY : Int = 36289;
	static inline var SAMPLER_2D_ARRAY_SHADOW : Int = 36292;
	static inline var SAMPLER_CUBE_SHADOW : Int = 36293;
	static inline var UNSIGNED_INT_VEC2 : Int = 36294;
	static inline var UNSIGNED_INT_VEC3 : Int = 36295;
	static inline var UNSIGNED_INT_VEC4 : Int = 36296;
	static inline var INT_SAMPLER_2D : Int = 36298;
	static inline var INT_SAMPLER_3D : Int = 36299;
	static inline var INT_SAMPLER_CUBE : Int = 36300;
	static inline var INT_SAMPLER_2D_ARRAY : Int = 36303;
	static inline var UNSIGNED_INT_SAMPLER_2D : Int = 36306;
	static inline var UNSIGNED_INT_SAMPLER_3D : Int = 36307;
	static inline var UNSIGNED_INT_SAMPLER_CUBE : Int = 36308;
	static inline var UNSIGNED_INT_SAMPLER_2D_ARRAY : Int = 36311;
	static inline var DEPTH_COMPONENT32F : Int = 36012;
	static inline var DEPTH32F_STENCIL8 : Int = 36013;
	static inline var FLOAT_32_UNSIGNED_INT_24_8_REV : Int = 36269;
	static inline var FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING : Int = 33296;
	static inline var FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE : Int = 33297;
	static inline var FRAMEBUFFER_ATTACHMENT_RED_SIZE : Int = 33298;
	static inline var FRAMEBUFFER_ATTACHMENT_GREEN_SIZE : Int = 33299;
	static inline var FRAMEBUFFER_ATTACHMENT_BLUE_SIZE : Int = 33300;
	static inline var FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE : Int = 33301;
	static inline var FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE : Int = 33302;
	static inline var FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE : Int = 33303;
	static inline var FRAMEBUFFER_DEFAULT : Int = 33304;
	static inline var UNSIGNED_INT_24_8 : Int = 34042;
	static inline var DEPTH24_STENCIL8 : Int = 35056;
	static inline var UNSIGNED_NORMALIZED : Int = 35863;
	static inline var DRAW_FRAMEBUFFER_BINDING : Int = 36006;
	static inline var READ_FRAMEBUFFER : Int = 36008;
	static inline var DRAW_FRAMEBUFFER : Int = 36009;
	static inline var READ_FRAMEBUFFER_BINDING : Int = 36010;
	static inline var RENDERBUFFER_SAMPLES : Int = 36011;
	static inline var FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER : Int = 36052;
	static inline var MAX_COLOR_ATTACHMENTS : Int = 36063;
	static inline var COLOR_ATTACHMENT1 : Int = 36065;
	static inline var COLOR_ATTACHMENT2 : Int = 36066;
	static inline var COLOR_ATTACHMENT3 : Int = 36067;
	static inline var COLOR_ATTACHMENT4 : Int = 36068;
	static inline var COLOR_ATTACHMENT5 : Int = 36069;
	static inline var COLOR_ATTACHMENT6 : Int = 36070;
	static inline var COLOR_ATTACHMENT7 : Int = 36071;
	static inline var COLOR_ATTACHMENT8 : Int = 36072;
	static inline var COLOR_ATTACHMENT9 : Int = 36073;
	static inline var COLOR_ATTACHMENT10 : Int = 36074;
	static inline var COLOR_ATTACHMENT11 : Int = 36075;
	static inline var COLOR_ATTACHMENT12 : Int = 36076;
	static inline var COLOR_ATTACHMENT13 : Int = 36077;
	static inline var COLOR_ATTACHMENT14 : Int = 36078;
	static inline var COLOR_ATTACHMENT15 : Int = 36079;
	static inline var FRAMEBUFFER_INCOMPLETE_MULTISAMPLE : Int = 36182;
	static inline var MAX_SAMPLES : Int = 36183;
	static inline var HALF_FLOAT : Int = 5131;
	static inline var RG : Int = 33319;
	static inline var RG_INTEGER : Int = 33320;
	static inline var R8 : Int = 33321;
	static inline var RG8 : Int = 33323;
	static inline var R16F : Int = 33325;
	static inline var R32F : Int = 33326;
	static inline var RG16F : Int = 33327;
	static inline var RG32F : Int = 33328;
	static inline var R8I : Int = 33329;
	static inline var R8UI : Int = 33330;
	static inline var R16I : Int = 33331;
	static inline var R16UI : Int = 33332;
	static inline var R32I : Int = 33333;
	static inline var R32UI : Int = 33334;
	static inline var RG8I : Int = 33335;
	static inline var RG8UI : Int = 33336;
	static inline var RG16I : Int = 33337;
	static inline var RG16UI : Int = 33338;
	static inline var RG32I : Int = 33339;
	static inline var RG32UI : Int = 33340;
	static inline var VERTEX_ARRAY_BINDING : Int = 34229;
	static inline var R8_SNORM : Int = 36756;
	static inline var RG8_SNORM : Int = 36757;
	static inline var RGB8_SNORM : Int = 36758;
	static inline var RGBA8_SNORM : Int = 36759;
	static inline var SIGNED_NORMALIZED : Int = 36764;
	static inline var COPY_READ_BUFFER : Int = 36662;
	static inline var COPY_WRITE_BUFFER : Int = 36663;
	static inline var COPY_READ_BUFFER_BINDING : Int = 36662;
	static inline var COPY_WRITE_BUFFER_BINDING : Int = 36663;
	static inline var UNIFORM_BUFFER : Int = 35345;
	static inline var UNIFORM_BUFFER_BINDING : Int = 35368;
	static inline var UNIFORM_BUFFER_START : Int = 35369;
	static inline var UNIFORM_BUFFER_SIZE : Int = 35370;
	static inline var MAX_VERTEX_UNIFORM_BLOCKS : Int = 35371;
	static inline var MAX_FRAGMENT_UNIFORM_BLOCKS : Int = 35373;
	static inline var MAX_COMBINED_UNIFORM_BLOCKS : Int = 35374;
	static inline var MAX_UNIFORM_BUFFER_BINDINGS : Int = 35375;
	static inline var MAX_UNIFORM_BLOCK_SIZE : Int = 35376;
	static inline var MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS : Int = 35377;
	static inline var MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS : Int = 35379;
	static inline var UNIFORM_BUFFER_OFFSET_ALIGNMENT : Int = 35380;
	static inline var ACTIVE_UNIFORM_BLOCKS : Int = 35382;
	static inline var UNIFORM_TYPE : Int = 35383;
	static inline var UNIFORM_SIZE : Int = 35384;
	static inline var UNIFORM_BLOCK_INDEX : Int = 35386;
	static inline var UNIFORM_OFFSET : Int = 35387;
	static inline var UNIFORM_ARRAY_STRIDE : Int = 35388;
	static inline var UNIFORM_MATRIX_STRIDE : Int = 35389;
	static inline var UNIFORM_IS_ROW_MAJOR : Int = 35390;
	static inline var UNIFORM_BLOCK_BINDING : Int = 35391;
	static inline var UNIFORM_BLOCK_DATA_SIZE : Int = 35392;
	static inline var UNIFORM_BLOCK_ACTIVE_UNIFORMS : Int = 35394;
	static inline var UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES : Int = 35395;
	static inline var UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER : Int = 35396;
	static inline var UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER : Int = 35398;
	static inline var INVALID_INDEX : Int = cast 4294967295;
	static inline var MAX_VERTEX_OUTPUT_COMPONENTS : Int = 37154;
	static inline var MAX_FRAGMENT_INPUT_COMPONENTS : Int = 37157;
	static inline var MAX_SERVER_WAIT_TIMEOUT : Int = 37137;
	static inline var OBJECT_TYPE : Int = 37138;
	static inline var SYNC_CONDITION : Int = 37139;
	static inline var SYNC_STATUS : Int = 37140;
	static inline var SYNC_FLAGS : Int = 37141;
	static inline var SYNC_FENCE : Int = 37142;
	static inline var SYNC_GPU_COMMANDS_COMPLETE : Int = 37143;
	static inline var UNSIGNALED : Int = 37144;
	static inline var SIGNALED : Int = 37145;
	static inline var ALREADY_SIGNALED : Int = 37146;
	static inline var TIMEOUT_EXPIRED : Int = 37147;
	static inline var CONDITION_SATISFIED : Int = 37148;
	static inline var WAIT_FAILED : Int = 37149;
	static inline var SYNC_FLUSH_COMMANDS_BIT : Int = 1;
	static inline var VERTEX_ATTRIB_ARRAY_DIVISOR : Int = 35070;
	static inline var ANY_SAMPLES_PASSED : Int = 35887;
	static inline var ANY_SAMPLES_PASSED_CONSERVATIVE : Int = 36202;
	static inline var SAMPLER_BINDING : Int = 35097;
	static inline var RGB10_A2UI : Int = 36975;
	static inline var INT_2_10_10_10_REV : Int = 36255;
	static inline var TRANSFORM_FEEDBACK : Int = 36386;
	static inline var TRANSFORM_FEEDBACK_PAUSED : Int = 36387;
	static inline var TRANSFORM_FEEDBACK_ACTIVE : Int = 36388;
	static inline var TRANSFORM_FEEDBACK_BINDING : Int = 36389;
	static inline var TEXTURE_IMMUTABLE_FORMAT : Int = 37167;
	static inline var MAX_ELEMENT_INDEX : Int = 36203;
	static inline var TEXTURE_IMMUTABLE_LEVELS : Int = 33503;
	static inline var TIMEOUT_IGNORED : Int = -1;
	static inline var MAX_CLIENT_WAIT_TIMEOUT_WEBGL : Int = 37447;
	static inline var DEPTH_BUFFER_BIT : Int = 256;
	static inline var STENCIL_BUFFER_BIT : Int = 1024;
	static inline var COLOR_BUFFER_BIT : Int = 16384;
	static inline var POINTS : Int = 0;
	static inline var LINES : Int = 1;
	static inline var LINE_LOOP : Int = 2;
	static inline var LINE_STRIP : Int = 3;
	static inline var TRIANGLES : Int = 4;
	static inline var TRIANGLE_STRIP : Int = 5;
	static inline var TRIANGLE_FAN : Int = 6;
	static inline var ZERO : Int = 0;
	static inline var ONE : Int = 1;
	static inline var SRC_COLOR : Int = 768;
	static inline var ONE_MINUS_SRC_COLOR : Int = 769;
	static inline var SRC_ALPHA : Int = 770;
	static inline var ONE_MINUS_SRC_ALPHA : Int = 771;
	static inline var DST_ALPHA : Int = 772;
	static inline var ONE_MINUS_DST_ALPHA : Int = 773;
	static inline var DST_COLOR : Int = 774;
	static inline var ONE_MINUS_DST_COLOR : Int = 775;
	static inline var SRC_ALPHA_SATURATE : Int = 776;
	static inline var FUNC_ADD : Int = 32774;
	static inline var BLEND_EQUATION : Int = 32777;
	static inline var BLEND_EQUATION_RGB : Int = 32777;
	static inline var BLEND_EQUATION_ALPHA : Int = 34877;
	static inline var FUNC_SUBTRACT : Int = 32778;
	static inline var FUNC_REVERSE_SUBTRACT : Int = 32779;
	static inline var BLEND_DST_RGB : Int = 32968;
	static inline var BLEND_SRC_RGB : Int = 32969;
	static inline var BLEND_DST_ALPHA : Int = 32970;
	static inline var BLEND_SRC_ALPHA : Int = 32971;
	static inline var CONSTANT_COLOR : Int = 32769;
	static inline var ONE_MINUS_CONSTANT_COLOR : Int = 32770;
	static inline var CONSTANT_ALPHA : Int = 32771;
	static inline var ONE_MINUS_CONSTANT_ALPHA : Int = 32772;
	static inline var BLEND_COLOR : Int = 32773;
	static inline var ARRAY_BUFFER : Int = 34962;
	static inline var ELEMENT_ARRAY_BUFFER : Int = 34963;
	static inline var ARRAY_BUFFER_BINDING : Int = 34964;
	static inline var ELEMENT_ARRAY_BUFFER_BINDING : Int = 34965;
	static inline var STREAM_DRAW : Int = 35040;
	static inline var STATIC_DRAW : Int = 35044;
	static inline var DYNAMIC_DRAW : Int = 35048;
	static inline var BUFFER_SIZE : Int = 34660;
	static inline var BUFFER_USAGE : Int = 34661;
	static inline var CURRENT_VERTEX_ATTRIB : Int = 34342;
	static inline var FRONT : Int = 1028;
	static inline var BACK : Int = 1029;
	static inline var FRONT_AND_BACK : Int = 1032;
	static inline var CULL_FACE : Int = 2884;
	static inline var BLEND : Int = 3042;
	static inline var DITHER : Int = 3024;
	static inline var STENCIL_TEST : Int = 2960;
	static inline var DEPTH_TEST : Int = 2929;
	static inline var SCISSOR_TEST : Int = 3089;
	static inline var POLYGON_OFFSET_FILL : Int = 32823;
	static inline var SAMPLE_ALPHA_TO_COVERAGE : Int = 32926;
	static inline var SAMPLE_COVERAGE : Int = 32928;
	static inline var NO_ERROR : Int = 0;
	static inline var INVALID_ENUM : Int = 1280;
	static inline var INVALID_VALUE : Int = 1281;
	static inline var INVALID_OPERATION : Int = 1282;
	static inline var OUT_OF_MEMORY : Int = 1285;
	static inline var CW : Int = 2304;
	static inline var CCW : Int = 2305;
	static inline var LINE_WIDTH : Int = 2849;
	static inline var ALIASED_POINT_SIZE_RANGE : Int = 33901;
	static inline var ALIASED_LINE_WIDTH_RANGE : Int = 33902;
	static inline var CULL_FACE_MODE : Int = 2885;
	static inline var FRONT_FACE : Int = 2886;
	static inline var DEPTH_RANGE : Int = 2928;
	static inline var DEPTH_WRITEMASK : Int = 2930;
	static inline var DEPTH_CLEAR_VALUE : Int = 2931;
	static inline var DEPTH_FUNC : Int = 2932;
	static inline var STENCIL_CLEAR_VALUE : Int = 2961;
	static inline var STENCIL_FUNC : Int = 2962;
	static inline var STENCIL_FAIL : Int = 2964;
	static inline var STENCIL_PASS_DEPTH_FAIL : Int = 2965;
	static inline var STENCIL_PASS_DEPTH_PASS : Int = 2966;
	static inline var STENCIL_REF : Int = 2967;
	static inline var STENCIL_VALUE_MASK : Int = 2963;
	static inline var STENCIL_WRITEMASK : Int = 2968;
	static inline var STENCIL_BACK_FUNC : Int = 34816;
	static inline var STENCIL_BACK_FAIL : Int = 34817;
	static inline var STENCIL_BACK_PASS_DEPTH_FAIL : Int = 34818;
	static inline var STENCIL_BACK_PASS_DEPTH_PASS : Int = 34819;
	static inline var STENCIL_BACK_REF : Int = 36003;
	static inline var STENCIL_BACK_VALUE_MASK : Int = 36004;
	static inline var STENCIL_BACK_WRITEMASK : Int = 36005;
	static inline var VIEWPORT : Int = 2978;
	static inline var SCISSOR_BOX : Int = 3088;
	static inline var COLOR_CLEAR_VALUE : Int = 3106;
	static inline var COLOR_WRITEMASK : Int = 3107;
	static inline var UNPACK_ALIGNMENT : Int = 3317;
	static inline var PACK_ALIGNMENT : Int = 3333;
	static inline var MAX_TEXTURE_SIZE : Int = 3379;
	static inline var MAX_VIEWPORT_DIMS : Int = 3386;
	static inline var SUBPIXEL_BITS : Int = 3408;
	static inline var RED_BITS : Int = 3410;
	static inline var GREEN_BITS : Int = 3411;
	static inline var BLUE_BITS : Int = 3412;
	static inline var ALPHA_BITS : Int = 3413;
	static inline var DEPTH_BITS : Int = 3414;
	static inline var STENCIL_BITS : Int = 3415;
	static inline var POLYGON_OFFSET_UNITS : Int = 10752;
	static inline var POLYGON_OFFSET_FACTOR : Int = 32824;
	static inline var TEXTURE_BINDING_2D : Int = 32873;
	static inline var SAMPLE_BUFFERS : Int = 32936;
	static inline var SAMPLES : Int = 32937;
	static inline var SAMPLE_COVERAGE_VALUE : Int = 32938;
	static inline var SAMPLE_COVERAGE_INVERT : Int = 32939;
	static inline var COMPRESSED_TEXTURE_FORMATS : Int = 34467;
	static inline var DONT_CARE : Int = 4352;
	static inline var FASTEST : Int = 4353;
	static inline var NICEST : Int = 4354;
	static inline var GENERATE_MIPMAP_HINT : Int = 33170;
	static inline var BYTE : Int = 5120;
	static inline var UNSIGNED_BYTE : Int = 5121;
	static inline var SHORT : Int = 5122;
	static inline var UNSIGNED_SHORT : Int = 5123;
	static inline var INT : Int = 5124;
	static inline var UNSIGNED_INT : Int = 5125;
	static inline var FLOAT : Int = 5126;
	static inline var DEPTH_COMPONENT : Int = 6402;
	static inline var ALPHA : Int = 6406;
	static inline var RGB : Int = 6407;
	static inline var RGBA : Int = 6408;
	static inline var LUMINANCE : Int = 6409;
	static inline var LUMINANCE_ALPHA : Int = 6410;
	static inline var UNSIGNED_SHORT_4_4_4_4 : Int = 32819;
	static inline var UNSIGNED_SHORT_5_5_5_1 : Int = 32820;
	static inline var UNSIGNED_SHORT_5_6_5 : Int = 33635;
	static inline var FRAGMENT_SHADER : Int = 35632;
	static inline var VERTEX_SHADER : Int = 35633;
	static inline var MAX_VERTEX_ATTRIBS : Int = 34921;
	static inline var MAX_VERTEX_UNIFORM_VECTORS : Int = 36347;
	static inline var MAX_VARYING_VECTORS : Int = 36348;
	static inline var MAX_COMBINED_TEXTURE_IMAGE_UNITS : Int = 35661;
	static inline var MAX_VERTEX_TEXTURE_IMAGE_UNITS : Int = 35660;
	static inline var MAX_TEXTURE_IMAGE_UNITS : Int = 34930;
	static inline var MAX_FRAGMENT_UNIFORM_VECTORS : Int = 36349;
	static inline var SHADER_TYPE : Int = 35663;
	static inline var DELETE_STATUS : Int = 35712;
	static inline var LINK_STATUS : Int = 35714;
	static inline var VALIDATE_STATUS : Int = 35715;
	static inline var ATTACHED_SHADERS : Int = 35717;
	static inline var ACTIVE_UNIFORMS : Int = 35718;
	static inline var ACTIVE_ATTRIBUTES : Int = 35721;
	static inline var SHADING_LANGUAGE_VERSION : Int = 35724;
	static inline var CURRENT_PROGRAM : Int = 35725;
	static inline var NEVER : Int = 512;
	static inline var LESS : Int = 513;
	static inline var EQUAL : Int = 514;
	static inline var LEQUAL : Int = 515;
	static inline var GREATER : Int = 516;
	static inline var NOTEQUAL : Int = 517;
	static inline var GEQUAL : Int = 518;
	static inline var ALWAYS : Int = 519;
	static inline var KEEP : Int = 7680;
	static inline var REPLACE : Int = 7681;
	static inline var INCR : Int = 7682;
	static inline var DECR : Int = 7683;
	static inline var INVERT : Int = 5386;
	static inline var INCR_WRAP : Int = 34055;
	static inline var DECR_WRAP : Int = 34056;
	static inline var VENDOR : Int = 7936;
	static inline var RENDERER : Int = 7937;
	static inline var VERSION : Int = 7938;
	static inline var NEAREST : Int = 9728;
	static inline var LINEAR : Int = 9729;
	static inline var NEAREST_MIPMAP_NEAREST : Int = 9984;
	static inline var LINEAR_MIPMAP_NEAREST : Int = 9985;
	static inline var NEAREST_MIPMAP_LINEAR : Int = 9986;
	static inline var LINEAR_MIPMAP_LINEAR : Int = 9987;
	static inline var TEXTURE_MAG_FILTER : Int = 10240;
	static inline var TEXTURE_MIN_FILTER : Int = 10241;
	static inline var TEXTURE_WRAP_S : Int = 10242;
	static inline var TEXTURE_WRAP_T : Int = 10243;
	static inline var TEXTURE_2D : Int = 3553;
	static inline var TEXTURE : Int = 5890;
	static inline var TEXTURE_CUBE_MAP : Int = 34067;
	static inline var TEXTURE_BINDING_CUBE_MAP : Int = 34068;
	static inline var TEXTURE_CUBE_MAP_POSITIVE_X : Int = 34069;
	static inline var TEXTURE_CUBE_MAP_NEGATIVE_X : Int = 34070;
	static inline var TEXTURE_CUBE_MAP_POSITIVE_Y : Int = 34071;
	static inline var TEXTURE_CUBE_MAP_NEGATIVE_Y : Int = 34072;
	static inline var TEXTURE_CUBE_MAP_POSITIVE_Z : Int = 34073;
	static inline var TEXTURE_CUBE_MAP_NEGATIVE_Z : Int = 34074;
	static inline var MAX_CUBE_MAP_TEXTURE_SIZE : Int = 34076;
	static inline var TEXTURE0 : Int = 33984;
	static inline var TEXTURE1 : Int = 33985;
	static inline var TEXTURE2 : Int = 33986;
	static inline var TEXTURE3 : Int = 33987;
	static inline var TEXTURE4 : Int = 33988;
	static inline var TEXTURE5 : Int = 33989;
	static inline var TEXTURE6 : Int = 33990;
	static inline var TEXTURE7 : Int = 33991;
	static inline var TEXTURE8 : Int = 33992;
	static inline var TEXTURE9 : Int = 33993;
	static inline var TEXTURE10 : Int = 33994;
	static inline var TEXTURE11 : Int = 33995;
	static inline var TEXTURE12 : Int = 33996;
	static inline var TEXTURE13 : Int = 33997;
	static inline var TEXTURE14 : Int = 33998;
	static inline var TEXTURE15 : Int = 33999;
	static inline var TEXTURE16 : Int = 34000;
	static inline var TEXTURE17 : Int = 34001;
	static inline var TEXTURE18 : Int = 34002;
	static inline var TEXTURE19 : Int = 34003;
	static inline var TEXTURE20 : Int = 34004;
	static inline var TEXTURE21 : Int = 34005;
	static inline var TEXTURE22 : Int = 34006;
	static inline var TEXTURE23 : Int = 34007;
	static inline var TEXTURE24 : Int = 34008;
	static inline var TEXTURE25 : Int = 34009;
	static inline var TEXTURE26 : Int = 34010;
	static inline var TEXTURE27 : Int = 34011;
	static inline var TEXTURE28 : Int = 34012;
	static inline var TEXTURE29 : Int = 34013;
	static inline var TEXTURE30 : Int = 34014;
	static inline var TEXTURE31 : Int = 34015;
	static inline var ACTIVE_TEXTURE : Int = 34016;
	static inline var REPEAT : Int = 10497;
	static inline var CLAMP_TO_EDGE : Int = 33071;
	static inline var MIRRORED_REPEAT : Int = 33648;
	static inline var FLOAT_VEC2 : Int = 35664;
	static inline var FLOAT_VEC3 : Int = 35665;
	static inline var FLOAT_VEC4 : Int = 35666;
	static inline var INT_VEC2 : Int = 35667;
	static inline var INT_VEC3 : Int = 35668;
	static inline var INT_VEC4 : Int = 35669;
	static inline var BOOL : Int = 35670;
	static inline var BOOL_VEC2 : Int = 35671;
	static inline var BOOL_VEC3 : Int = 35672;
	static inline var BOOL_VEC4 : Int = 35673;
	static inline var FLOAT_MAT2 : Int = 35674;
	static inline var FLOAT_MAT3 : Int = 35675;
	static inline var FLOAT_MAT4 : Int = 35676;
	static inline var SAMPLER_2D : Int = 35678;
	static inline var SAMPLER_CUBE : Int = 35680;
	static inline var VERTEX_ATTRIB_ARRAY_ENABLED : Int = 34338;
	static inline var VERTEX_ATTRIB_ARRAY_SIZE : Int = 34339;
	static inline var VERTEX_ATTRIB_ARRAY_STRIDE : Int = 34340;
	static inline var VERTEX_ATTRIB_ARRAY_TYPE : Int = 34341;
	static inline var VERTEX_ATTRIB_ARRAY_NORMALIZED : Int = 34922;
	static inline var VERTEX_ATTRIB_ARRAY_POINTER : Int = 34373;
	static inline var VERTEX_ATTRIB_ARRAY_BUFFER_BINDING : Int = 34975;
	static inline var IMPLEMENTATION_COLOR_READ_TYPE : Int = 35738;
	static inline var IMPLEMENTATION_COLOR_READ_FORMAT : Int = 35739;
	static inline var COMPILE_STATUS : Int = 35713;
	static inline var LOW_FLOAT : Int = 36336;
	static inline var MEDIUM_FLOAT : Int = 36337;
	static inline var HIGH_FLOAT : Int = 36338;
	static inline var LOW_INT : Int = 36339;
	static inline var MEDIUM_INT : Int = 36340;
	static inline var HIGH_INT : Int = 36341;
	static inline var FRAMEBUFFER : Int = 36160;
	static inline var RENDERBUFFER : Int = 36161;
	static inline var RGBA4 : Int = 32854;
	static inline var RGB5_A1 : Int = 32855;
	static inline var RGB565 : Int = 36194;
	static inline var DEPTH_COMPONENT16 : Int = 33189;
	static inline var STENCIL_INDEX8 : Int = 36168;
	static inline var DEPTH_STENCIL : Int = 34041;
	static inline var RENDERBUFFER_WIDTH : Int = 36162;
	static inline var RENDERBUFFER_HEIGHT : Int = 36163;
	static inline var RENDERBUFFER_INTERNAL_FORMAT : Int = 36164;
	static inline var RENDERBUFFER_RED_SIZE : Int = 36176;
	static inline var RENDERBUFFER_GREEN_SIZE : Int = 36177;
	static inline var RENDERBUFFER_BLUE_SIZE : Int = 36178;
	static inline var RENDERBUFFER_ALPHA_SIZE : Int = 36179;
	static inline var RENDERBUFFER_DEPTH_SIZE : Int = 36180;
	static inline var RENDERBUFFER_STENCIL_SIZE : Int = 36181;
	static inline var FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE : Int = 36048;
	static inline var FRAMEBUFFER_ATTACHMENT_OBJECT_NAME : Int = 36049;
	static inline var FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL : Int = 36050;
	static inline var FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE : Int = 36051;
	static inline var COLOR_ATTACHMENT0 : Int = 36064;
	static inline var DEPTH_ATTACHMENT : Int = 36096;
	static inline var STENCIL_ATTACHMENT : Int = 36128;
	static inline var DEPTH_STENCIL_ATTACHMENT : Int = 33306;
	static inline var NONE : Int = 0;
	static inline var FRAMEBUFFER_COMPLETE : Int = 36053;
	static inline var FRAMEBUFFER_INCOMPLETE_ATTACHMENT : Int = 36054;
	static inline var FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT : Int = 36055;
	static inline var FRAMEBUFFER_INCOMPLETE_DIMENSIONS : Int = 36057;
	static inline var FRAMEBUFFER_UNSUPPORTED : Int = 36061;
	static inline var FRAMEBUFFER_BINDING : Int = 36006;
	static inline var RENDERBUFFER_BINDING : Int = 36007;
	static inline var MAX_RENDERBUFFER_SIZE : Int = 34024;
	static inline var INVALID_FRAMEBUFFER_OPERATION : Int = 1286;
	static inline var UNPACK_FLIP_Y_WEBGL : Int = 37440;
	static inline var UNPACK_PREMULTIPLY_ALPHA_WEBGL : Int = 37441;
	static inline var CONTEXT_LOST_WEBGL : Int = 37442;
	static inline var UNPACK_COLORSPACE_CONVERSION_WEBGL : Int = 37443;
	static inline var BROWSER_DEFAULT_WEBGL : Int = 37444;
	
	
	/**
		Initializes and creates the buffer object's data store.
	**/
	@:overload( function( target : Int, size : Int, usage : Int ) : Void {} )
	@:overload( function( target : Int, srcData : js.lib.ArrayBuffer, usage : Int ) : Void {} )
	@:overload( function( target : Int, srcData : js.lib.ArrayBufferView, usage : Int ) : Void {} )
	function bufferData( target : Int, srcData : js.lib.ArrayBufferView, usage : Int, srcOffset : Int, length : Int = 0 ) : Void;
	
	/**
		Updates a subset of a buffer object's data store.
	**/
	@:overload( function( target : Int, offset : Int, srcData : js.lib.ArrayBuffer ) : Void {} )
	@:overload( function( target : Int, offset : Int, srcData : js.lib.ArrayBufferView ) : Void {} )
	function bufferSubData( target : Int, dstByteOffset : Int, srcData : js.lib.ArrayBufferView, srcOffset : Int, length : Int = 0 ) : Void;
	
	/**
		Copies part of the data of a buffer to another buffer.
	**/
	function copyBufferSubData( readTarget : Int, writeTarget : Int, readOffset : Int, writeOffset : Int, size : Int ) : Void;
	
	/**
		Reads data from a buffer and writes them to an `ArrayBuffer` or `SharedArrayBuffer`.
	**/
	function getBufferSubData( target : Int, srcByteOffset : Int, dstData : js.lib.ArrayBufferView, dstOffset : Int = 0, length : Int = 0 ) : Void;
	
	/**
		Transfers a block of pixels from the read framebuffer to the draw framebuffer.
	**/
	function blitFramebuffer( srcX0 : Int, srcY0 : Int, srcX1 : Int, srcY1 : Int, dstX0 : Int, dstY0 : Int, dstX1 : Int, dstY1 : Int, mask : Int, filter : Int ) : Void;
	
	/**
		Attaches a single layer of a texture to a framebuffer.
	**/
	function framebufferTextureLayer( target : Int, attachment : Int, texture : Texture, level : Int, layer : Int ) : Void;
	
	/**
		Invalidates the contents of attachments in a framebuffer.
		@throws DOMError
	**/
	function invalidateFramebuffer( target : Int, attachments : Array<Int> ) : Void;
	
	/**
		Invalidates portions of the contents of attachments in a framebuffer
		@throws DOMError
	**/
	function invalidateSubFramebuffer( target : Int, attachments : Array<Int>, x : Int, y : Int, width : Int, height : Int ) : Void;
	
	/**
		Selects a color buffer as the source for pixels.
	**/
	function readBuffer( src : Int ) : Void;
	
	/**
		Returns information about implementation-dependent support for internal formats.
		@throws DOMError
	**/
	function getInternalformatParameter( target : Int, internalformat : Int, pname : Int ) : Dynamic;
	
	/**
		Creates and initializes a renderbuffer object's data store and allows specifying the number of samples to be used.
	**/
	function renderbufferStorageMultisample( target : Int, samples : Int, internalformat : Int, width : Int, height : Int ) : Void;
	
	/**
		Specifies all levels of two-dimensional texture storage.
	**/
	function texStorage2D( target : Int, levels : Int, internalformat : Int, width : Int, height : Int ) : Void;
	
	/**
		Specifies all levels of a three-dimensional texture or two-dimensional array texture.
	**/
	function texStorage3D( target : Int, levels : Int, internalformat : Int, width : Int, height : Int, depth : Int ) : Void;
	/** @throws DOMError */
	@:overload( function( target : Int, level : Int, internalformat : Int, width : Int, height : Int, border : Int, format : Int, type : Int, pixels : js.lib.ArrayBufferView ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, format : Int, type : Int, source : js.html.CanvasElement ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, format : Int, type : Int, source : js.html.ImageElement ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, format : Int, type : Int, source : js.html.VideoElement ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, format : Int, type : Int, source : js.html.ImageBitmap ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, format : Int, type : Int, source : js.html.ImageData ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, width : Int, height : Int, border : Int, format : Int, type : Int, pboOffset : Int ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, width : Int, height : Int, border : Int, format : Int, type : Int, source : js.html.CanvasElement ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, width : Int, height : Int, border : Int, format : Int, type : Int, source : js.html.ImageElement ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, width : Int, height : Int, border : Int, format : Int, type : Int, source : js.html.VideoElement ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, width : Int, height : Int, border : Int, format : Int, type : Int, source : js.html.ImageBitmap ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, width : Int, height : Int, border : Int, format : Int, type : Int, source : js.html.ImageData ) : Void {} )
	function texImage2D( target : Int, level : Int, internalformat : Int, width : Int, height : Int, border : Int, format : Int, type : Int, srcData : js.lib.ArrayBufferView, srcOffset : Int ) : Void;
	/** @throws DOMError */
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, width : Int, height : Int, format : Int, type : Int, pixels : js.lib.ArrayBufferView ) : Void {} )
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, format : Int, type : Int, source : js.html.CanvasElement ) : Void {} )
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, format : Int, type : Int, source : js.html.ImageElement ) : Void {} )
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, format : Int, type : Int, source : js.html.VideoElement ) : Void {} )
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, format : Int, type : Int, source : js.html.ImageBitmap ) : Void {} )
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, format : Int, type : Int, source : js.html.ImageData ) : Void {} )
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, width : Int, height : Int, format : Int, type : Int, pboOffset : Int ) : Void {} )
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, width : Int, height : Int, format : Int, type : Int, source : js.html.CanvasElement ) : Void {} )
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, width : Int, height : Int, format : Int, type : Int, source : js.html.ImageElement ) : Void {} )
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, width : Int, height : Int, format : Int, type : Int, source : js.html.VideoElement ) : Void {} )
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, width : Int, height : Int, format : Int, type : Int, source : js.html.ImageBitmap ) : Void {} )
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, width : Int, height : Int, format : Int, type : Int, source : js.html.ImageData ) : Void {} )
	function texSubImage2D( target : Int, level : Int, xoffset : Int, yoffset : Int, width : Int, height : Int, format : Int, type : Int, srcData : js.lib.ArrayBufferView, srcOffset : Int ) : Void;
	
	/**
		Specifies a three-dimensional texture image.
		@throws DOMError
	**/
	@:overload( function( target : Int, level : Int, internalformat : Int, width : Int, height : Int, depth : Int, border : Int, format : Int, type : Int, pboOffset : Int ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, width : Int, height : Int, depth : Int, border : Int, format : Int, type : Int, source : js.html.CanvasElement ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, width : Int, height : Int, depth : Int, border : Int, format : Int, type : Int, source : js.html.ImageElement ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, width : Int, height : Int, depth : Int, border : Int, format : Int, type : Int, source : js.html.VideoElement ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, width : Int, height : Int, depth : Int, border : Int, format : Int, type : Int, source : js.html.ImageBitmap ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, width : Int, height : Int, depth : Int, border : Int, format : Int, type : Int, source : js.html.ImageData ) : Void {} )
	@:overload( function( target : Int, level : Int, internalformat : Int, width : Int, height : Int, depth : Int, border : Int, format : Int, type : Int, srcData : js.lib.ArrayBufferView ) : Void {} )
	function texImage3D( target : Int, level : Int, internalformat : Int, width : Int, height : Int, depth : Int, border : Int, format : Int, type : Int, srcData : js.lib.ArrayBufferView, srcOffset : Int ) : Void;
	
	/**
		Specifies a sub-rectangle of the current 3D texture.
		@throws DOMError
	**/
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, zoffset : Int, width : Int, height : Int, depth : Int, format : Int, type : Int, pboOffset : Int ) : Void {} )
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, zoffset : Int, width : Int, height : Int, depth : Int, format : Int, type : Int, source : js.html.CanvasElement ) : Void {} )
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, zoffset : Int, width : Int, height : Int, depth : Int, format : Int, type : Int, source : js.html.ImageElement ) : Void {} )
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, zoffset : Int, width : Int, height : Int, depth : Int, format : Int, type : Int, source : js.html.VideoElement ) : Void {} )
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, zoffset : Int, width : Int, height : Int, depth : Int, format : Int, type : Int, source : js.html.ImageBitmap ) : Void {} )
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, zoffset : Int, width : Int, height : Int, depth : Int, format : Int, type : Int, source : js.html.ImageData ) : Void {} )
	function texSubImage3D( target : Int, level : Int, xoffset : Int, yoffset : Int, zoffset : Int, width : Int, height : Int, depth : Int, format : Int, type : Int, srcData : js.lib.ArrayBufferView, srcOffset : Int = 0 ) : Void;
	
	/**
		Copies pixels from the current `WebGLFramebuffer` into an existing 3D texture sub-image.
	**/
	function copyTexSubImage3D( target : Int, level : Int, xoffset : Int, yoffset : Int, zoffset : Int, x : Int, y : Int, width : Int, height : Int ) : Void;
	@:overload( function( target : Int, level : Int, internalformat : Int, width : Int, height : Int, border : Int, imageSize : Int, offset : Int ) : Void {} )
	function compressedTexImage2D( target : Int, level : Int, internalformat : Int, width : Int, height : Int, border : Int, srcData : js.lib.ArrayBufferView, srcOffset : Int = 0, srcLengthOverride : Int = 0 ) : Void;
	
	/**
		Specifies a three-dimensional texture image in a compressed format.
	**/
	@:overload( function( target : Int, level : Int, internalformat : Int, width : Int, height : Int, depth : Int, border : Int, imageSize : Int, offset : Int ) : Void {} )
	function compressedTexImage3D( target : Int, level : Int, internalformat : Int, width : Int, height : Int, depth : Int, border : Int, srcData : js.lib.ArrayBufferView, srcOffset : Int = 0, srcLengthOverride : Int = 0 ) : Void;
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, width : Int, height : Int, format : Int, imageSize : Int, offset : Int ) : Void {} )
	function compressedTexSubImage2D( target : Int, level : Int, xoffset : Int, yoffset : Int, width : Int, height : Int, format : Int, srcData : js.lib.ArrayBufferView, srcOffset : Int = 0, srcLengthOverride : Int = 0 ) : Void;
	
	/**
		Specifies a three-dimensional sub-rectangle for a texture image in a compressed format.
	**/
	@:overload( function( target : Int, level : Int, xoffset : Int, yoffset : Int, zoffset : Int, width : Int, height : Int, depth : Int, format : Int, imageSize : Int, offset : Int ) : Void {} )
	function compressedTexSubImage3D( target : Int, level : Int, xoffset : Int, yoffset : Int, zoffset : Int, width : Int, height : Int, depth : Int, format : Int, srcData : js.lib.ArrayBufferView, srcOffset : Int = 0, srcLengthOverride : Int = 0 ) : Void;
	
	/**
		Returns the binding of color numbers to user-defined varying out variables.
	**/
	function getFragDataLocation( program : Program, name : String ) : Int;
	function uniform1ui( location : UniformLocation, v0 : Int ) : Void;
	function uniform2ui( location : UniformLocation, v0 : Int, v1 : Int ) : Void;
	function uniform3ui( location : UniformLocation, v0 : Int, v1 : Int, v2 : Int ) : Void;
	function uniform4ui( location : UniformLocation, v0 : Int, v1 : Int, v2 : Int, v3 : Int ) : Void;
	@:overload( function( location : UniformLocation, data : Array<Float>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniform1fv( location : UniformLocation, data : js.lib.Float32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, data : Array<Float>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniform2fv( location : UniformLocation, data : js.lib.Float32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, data : Array<Float>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniform3fv( location : UniformLocation, data : js.lib.Float32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, data : Array<Float>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniform4fv( location : UniformLocation, data : js.lib.Float32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, data : Array<Int>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniform1iv( location : UniformLocation, data : js.lib.Int32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, data : Array<Int>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniform2iv( location : UniformLocation, data : js.lib.Int32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, data : Array<Int>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniform3iv( location : UniformLocation, data : js.lib.Int32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, data : Array<Int>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniform4iv( location : UniformLocation, data : js.lib.Int32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, data : Array<Int>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniform1uiv( location : UniformLocation, data : js.lib.Uint32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, data : Array<Int>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniform2uiv( location : UniformLocation, data : js.lib.Uint32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, data : Array<Int>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniform3uiv( location : UniformLocation, data : js.lib.Uint32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, data : Array<Int>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniform4uiv( location : UniformLocation, data : js.lib.Uint32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, transpose : Bool, data : Array<Float>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniformMatrix2fv( location : UniformLocation, transpose : Bool, data : js.lib.Float32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, transpose : Bool, data : Array<Float>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniformMatrix3x2fv( location : UniformLocation, transpose : Bool, data : js.lib.Float32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, transpose : Bool, data : Array<Float>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniformMatrix4x2fv( location : UniformLocation, transpose : Bool, data : js.lib.Float32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, transpose : Bool, data : Array<Float>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniformMatrix2x3fv( location : UniformLocation, transpose : Bool, data : js.lib.Float32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, transpose : Bool, data : Array<Float>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniformMatrix3fv( location : UniformLocation, transpose : Bool, data : js.lib.Float32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, transpose : Bool, data : Array<Float>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniformMatrix4x3fv( location : UniformLocation, transpose : Bool, data : js.lib.Float32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, transpose : Bool, data : Array<Float>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniformMatrix2x4fv( location : UniformLocation, transpose : Bool, data : js.lib.Float32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, transpose : Bool, data : Array<Float>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniformMatrix3x4fv( location : UniformLocation, transpose : Bool, data : js.lib.Float32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	@:overload( function( location : UniformLocation, transpose : Bool, data : Array<Float>, srcOffset : Int = 0, srcLength : Int = 0) : Void {} )
	function uniformMatrix4fv( location : UniformLocation, transpose : Bool, data : js.lib.Float32Array, srcOffset : Int = 0, srcLength : Int = 0 ) : Void;
	function vertexAttribI4i( index : Int, x : Int, y : Int, z : Int, w : Int ) : Void;
	@:overload( function( index : Int, values : Array<Int>) : Void {} )
	function vertexAttribI4iv( index : Int, values : js.lib.Int32Array ) : Void;
	function vertexAttribI4ui( index : Int, x : Int, y : Int, z : Int, w : Int ) : Void;
	@:overload( function( index : Int, values : Array<Int>) : Void {} )
	function vertexAttribI4uiv( index : Int, values : js.lib.Uint32Array ) : Void;
	
	/**
		Specifies integer data formats and locations of vertex attributes in a vertex attributes array.
	**/
	function vertexAttribIPointer( index : Int, size : Int, type : Int, stride : Int, offset : Int ) : Void;
	
	/**
		Modifies the rate at which generic vertex attributes advance when rendering multiple instances of primitives with `WebGL2RenderingContext.drawArraysInstanced()` and `WebGL2RenderingContext.drawElementsInstanced()`.
	**/
	function vertexAttribDivisor( index : Int, divisor : Int ) : Void;
	
	/**
		Renders primitives from array data. In addition, it can execute multiple instances of the range of elements.
	**/
	function drawArraysInstanced( mode : Int, first : Int, count : Int, instanceCount : Int ) : Void;
	
	/**
		Renders primitives from array data. In addition, it can execute multiple instances of a set of elements.
	**/
	function drawElementsInstanced( mode : Int, count : Int, type : Int, offset : Int, instanceCount : Int ) : Void;
	
	/**
		Renders primitives from array data in a given range.
	**/
	function drawRangeElements( mode : Int, start : Int, end : Int, count : Int, type : Int, offset : Int ) : Void;
	/** @throws DOMError */
	@:overload( function( x : Int, y : Int, width : Int, height : Int, format : Int, type : Int, dstData : js.lib.ArrayBufferView ) : Void {} )
	@:overload( function( x : Int, y : Int, width : Int, height : Int, format : Int, type : Int, offset : Int ) : Void {} )
	function readPixels( x : Int, y : Int, width : Int, height : Int, format : Int, type : Int, dstData : js.lib.ArrayBufferView, dstOffset : Int ) : Void;
	
	/**
		Specifies a list of color buffers to be drawn into.
	**/
	function drawBuffers( buffers : Array<Int> ) : Void;
	@:overload( function( buffer : Int, drawbuffer : Int, values : Array<Float>, srcOffset : Int = 0) : Void {} )
	function clearBufferfv( buffer : Int, drawbuffer : Int, values : js.lib.Float32Array, srcOffset : Int = 0 ) : Void;
	@:overload( function( buffer : Int, drawbuffer : Int, values : Array<Int>, srcOffset : Int = 0) : Void {} )
	function clearBufferiv( buffer : Int, drawbuffer : Int, values : js.lib.Int32Array, srcOffset : Int = 0 ) : Void;
	@:overload( function( buffer : Int, drawbuffer : Int, values : Array<Int>, srcOffset : Int = 0) : Void {} )
	function clearBufferuiv( buffer : Int, drawbuffer : Int, values : js.lib.Uint32Array, srcOffset : Int = 0 ) : Void;
	function clearBufferfi( buffer : Int, drawbuffer : Int, depth : Float, stencil : Int ) : Void;
	
	/**
		Creates a new `WebGLQuery` object.
	**/
	function createQuery() : Query;
	
	/**
		Deletes a given `WebGLQuery` object.
	**/
	function deleteQuery( query : Query ) : Void;
	
	/**
		Returns `true` if a given object is a valid `WebGLQuery` object.
	**/
	function isQuery( query : Query ) : Bool;
	
	/**
		Begins an asynchronous query.
	**/
	function beginQuery( target : Int, query : Query ) : Void;
	
	/**
		Marks the end of an asynchronous query.
	**/
	function endQuery( target : Int ) : Void;
	
	/**
		Returns a `WebGLQuery` object for a given target.
	**/
	function getQuery( target : Int, pname : Int ) : Dynamic;
	
	/**
		Returns information about a query.
	**/
	function getQueryParameter( query : Query, pname : Int ) : Dynamic;
	
	/**
		Creates a new `WebGLSampler` object.
	**/
	function createSampler() : Sampler;
	
	/**
		Deletes a given `WebGLSampler` object.
	**/
	function deleteSampler( sampler : Sampler ) : Void;
	
	/**
		Returns `true` if a given object is a valid `WebGLSampler` object.
	**/
	function isSampler( sampler : Sampler ) : Bool;
	
	/**
		Binds a given `WebGLSampler` to a texture unit.
	**/
	function bindSampler( unit : Int, sampler : Sampler ) : Void;
	function samplerParameteri( sampler : Sampler, pname : Int, param : Int ) : Void;
	function samplerParameterf( sampler : Sampler, pname : Int, param : Float ) : Void;
	
	/**
		Returns sampler parameter information.
	**/
	function getSamplerParameter( sampler : Sampler, pname : Int ) : Dynamic;
	
	/**
		Creates a new `WebGLSync` object and inserts it into the GL command stream.
	**/
	function fenceSync( condition : Int, flags : Int ) : Sync;
	
	/**
		Returns `true` if the passed object is a valid `WebGLSync` object.
	**/
	function isSync( sync : Sync ) : Bool;
	
	/**
		Deletes a given `WebGLSync` object.
	**/
	function deleteSync( sync : Sync ) : Void;
	
	/**
		
		 Blocks and waits for a `WebGLSync` object to become signaled or a given timeout to be passed.
		 
	**/
	function clientWaitSync( sync : Sync, flags : Int, timeout : Int ) : Int;
	
	/**
		Returns immediately, but waits on the GL server until the given `WebGLSync` object is signaled.
	**/
	function waitSync( sync : Sync, flags : Int, timeout : Int ) : Void;
	
	/**
		Returns parameter information of a `WebGLSync` object.
	**/
	function getSyncParameter( sync : Sync, pname : Int ) : Dynamic;
	
	/**
		Creates and initializes `WebGLTransformFeedback` objects.
	**/
	function createTransformFeedback() : TransformFeedback;
	
	/**
		Deletes a given `WebGLTransformFeedback` object.
	**/
	function deleteTransformFeedback( tf : TransformFeedback ) : Void;
	
	/**
		Returns `true` if the passed object is a valid `WebGLTransformFeedback` object.
	**/
	function isTransformFeedback( tf : TransformFeedback ) : Bool;
	
	/**
		Binds a passed `WebGLTransformFeedback` object to the current GL state.
	**/
	function bindTransformFeedback( target : Int, tf : TransformFeedback ) : Void;
	
	/**
		Starts a transform feedback operation.
	**/
	function beginTransformFeedback( primitiveMode : Int ) : Void;
	
	/**
		Ends a transform feedback operation.
	**/
	function endTransformFeedback() : Void;
	
	/**
		Specifies values to record in `WebGLTransformFeedback` buffers.
	**/
	function transformFeedbackVaryings( program : Program, varyings : Array<String>, bufferMode : Int ) : Void;
	
	/**
		Returns information about varying variables from `WebGLTransformFeedback` buffers.
	**/
	function getTransformFeedbackVarying( program : Program, index : Int ) : ActiveInfo;
	
	/**
		Pauses a transform feedback operation.
	**/
	function pauseTransformFeedback() : Void;
	
	/**
		Resumes a transform feedback operation.
	**/
	function resumeTransformFeedback() : Void;
	
	/**
		Binds a given `WebGLBuffer` to a given binding point (`target`) at a given `index`.
	**/
	function bindBufferBase( target : Int, index : Int, buffer : Buffer ) : Void;
	
	/**
		Binds a range of a given `WebGLBuffer` to a given binding point (`target`) at a given `index`.
	**/
	function bindBufferRange( target : Int, index : Int, buffer : Buffer, offset : Int, size : Int ) : Void;
	
	/**
		Returns the indexed value for the given `target`.
		@throws DOMError
	**/
	function getIndexedParameter( target : Int, index : Int ) : Dynamic;
	
	/**
		
		 Retrieves the indices of a number of uniforms within a `WebGLProgram`.
		 
	**/
	function getUniformIndices( program : Program, uniformNames : Array<String> ) : Array<Int>;
	
	/**
		Retrieves information about active uniforms within a `WebGLProgram`.
	**/
	function getActiveUniforms( program : Program, uniformIndices : Array<Int>, pname : Int ) : Dynamic;
	
	/**
		Retrieves the index of a uniform block within a `WebGLProgram`.
	**/
	function getUniformBlockIndex( program : Program, uniformBlockName : String ) : Int;
	
	/**
		Retrieves information about an active uniform block within a `WebGLProgram`.
		@throws DOMError
	**/
	function getActiveUniformBlockParameter( program : Program, uniformBlockIndex : Int, pname : Int ) : Dynamic;
	
	/**
		Retrieves the name of the active uniform block at a given index within a `WebGLProgram`.
	**/
	function getActiveUniformBlockName( program : Program, uniformBlockIndex : Int ) : String;
	
	/**
		Assigns binding points for active uniform blocks.
	**/
	function uniformBlockBinding( program : Program, uniformBlockIndex : Int, uniformBlockBinding : Int ) : Void;
	
	/**
		Creates a new `WebGLVertexArrayObject`.
	**/
	function createVertexArray() : VertexArrayObject;
	
	/**
		Deletes a given `WebGLVertexArrayObject`.
	**/
	function deleteVertexArray( vertexArray : VertexArrayObject ) : Void;
	
	/**
		Returns `true` if a given object is a valid `WebGLVertexArrayObject`.
	**/
	function isVertexArray( vertexArray : VertexArrayObject ) : Bool;
	
	/**
		Binds a given `WebGLVertexArrayObject` to the buffer.
	**/
	function bindVertexArray( array : VertexArrayObject ) : Void;
	function getContextAttributes() : ContextAttributes;
	function isContextLost() : Bool;
	function getSupportedExtensions() : Array<String>;
	/** @throws DOMError */
	function getExtension<T>( name : Extension<T> ) : T;
	function activeTexture( texture : Int ) : Void;
	function attachShader( program : Program, shader : Shader ) : Void;
	function bindAttribLocation( program : Program, index : Int, name : String ) : Void;
	function bindBuffer( target : Int, buffer : Buffer ) : Void;
	function bindFramebuffer( target : Int, framebuffer : Framebuffer ) : Void;
	function bindRenderbuffer( target : Int, renderbuffer : Renderbuffer ) : Void;
	function bindTexture( target : Int, texture : Texture ) : Void;
	function blendColor( red : Float, green : Float, blue : Float, alpha : Float ) : Void;
	function blendEquation( mode : Int ) : Void;
	function blendEquationSeparate( modeRGB : Int, modeAlpha : Int ) : Void;
	function blendFunc( sfactor : Int, dfactor : Int ) : Void;
	function blendFuncSeparate( srcRGB : Int, dstRGB : Int, srcAlpha : Int, dstAlpha : Int ) : Void;
	function checkFramebufferStatus( target : Int ) : Int;
	function clear( mask : Int ) : Void;
	function clearColor( red : Float, green : Float, blue : Float, alpha : Float ) : Void;
	function clearDepth( depth : Float ) : Void;
	function clearStencil( s : Int ) : Void;
	function colorMask( red : Bool, green : Bool, blue : Bool, alpha : Bool ) : Void;
	function compileShader( shader : Shader ) : Void;
	function copyTexImage2D( target : Int, level : Int, internalformat : Int, x : Int, y : Int, width : Int, height : Int, border : Int ) : Void;
	function copyTexSubImage2D( target : Int, level : Int, xoffset : Int, yoffset : Int, x : Int, y : Int, width : Int, height : Int ) : Void;
	function createBuffer() : Buffer;
	function createFramebuffer() : Framebuffer;
	function createProgram() : Program;
	function createRenderbuffer() : Renderbuffer;
	function createShader( type : Int ) : Shader;
	function createTexture() : Texture;
	function cullFace( mode : Int ) : Void;
	function deleteBuffer( buffer : Buffer ) : Void;
	function deleteFramebuffer( framebuffer : Framebuffer ) : Void;
	function deleteProgram( program : Program ) : Void;
	function deleteRenderbuffer( renderbuffer : Renderbuffer ) : Void;
	function deleteShader( shader : Shader ) : Void;
	function deleteTexture( texture : Texture ) : Void;
	function depthFunc( func : Int ) : Void;
	function depthMask( flag : Bool ) : Void;
	function depthRange( zNear : Float, zFar : Float ) : Void;
	function detachShader( program : Program, shader : Shader ) : Void;
	function disable( cap : Int ) : Void;
	function disableVertexAttribArray( index : Int ) : Void;
	function drawArrays( mode : Int, first : Int, count : Int ) : Void;
	function drawElements( mode : Int, count : Int, type : Int, offset : Int ) : Void;
	function enable( cap : Int ) : Void;
	function enableVertexAttribArray( index : Int ) : Void;
	function finish() : Void;
	function flush() : Void;
	function framebufferRenderbuffer( target : Int, attachment : Int, renderbuffertarget : Int, renderbuffer : Renderbuffer ) : Void;
	function framebufferTexture2D( target : Int, attachment : Int, textarget : Int, texture : Texture, level : Int ) : Void;
	function frontFace( mode : Int ) : Void;
	function generateMipmap( target : Int ) : Void;
	function getActiveAttrib( program : Program, index : Int ) : ActiveInfo;
	function getActiveUniform( program : Program, index : Int ) : ActiveInfo;
	function getAttachedShaders( program : Program ) : Array<Shader>;
	function getAttribLocation( program : Program, name : String ) : Int;
	function getBufferParameter( target : Int, pname : Int ) : Dynamic;
	/** @throws DOMError */
	function getParameter( pname : Int ) : Dynamic;
	function getError() : Int;
	/** @throws DOMError */
	function getFramebufferAttachmentParameter( target : Int, attachment : Int, pname : Int ) : Dynamic;
	function getProgramParameter( program : Program, pname : Int ) : Dynamic;
	function getProgramInfoLog( program : Program ) : String;
	function getRenderbufferParameter( target : Int, pname : Int ) : Dynamic;
	function getShaderParameter( shader : Shader, pname : Int ) : Dynamic;
	function getShaderPrecisionFormat( shadertype : Int, precisiontype : Int ) : ShaderPrecisionFormat;
	function getShaderInfoLog( shader : Shader ) : String;
	function getShaderSource( shader : Shader ) : String;
	function getTexParameter( target : Int, pname : Int ) : Dynamic;
	function getUniform( program : Program, location : UniformLocation ) : Dynamic;
	function getUniformLocation( program : Program, name : String ) : UniformLocation;
	/** @throws DOMError */
	function getVertexAttrib( index : Int, pname : Int ) : Dynamic;
	function getVertexAttribOffset( index : Int, pname : Int ) : Int;
	function hint( target : Int, mode : Int ) : Void;
	function isBuffer( buffer : Buffer ) : Bool;
	function isEnabled( cap : Int ) : Bool;
	function isFramebuffer( framebuffer : Framebuffer ) : Bool;
	function isProgram( program : Program ) : Bool;
	function isRenderbuffer( renderbuffer : Renderbuffer ) : Bool;
	function isShader( shader : Shader ) : Bool;
	function isTexture( texture : Texture ) : Bool;
	function lineWidth( width : Float ) : Void;
	function linkProgram( program : Program ) : Void;
	function pixelStorei( pname : Int, param : Int ) : Void;
	function polygonOffset( factor : Float, units : Float ) : Void;
	function renderbufferStorage( target : Int, internalformat : Int, width : Int, height : Int ) : Void;
	function sampleCoverage( value : Float, invert : Bool ) : Void;
	function scissor( x : Int, y : Int, width : Int, height : Int ) : Void;
	function shaderSource( shader : Shader, source : String ) : Void;
	function stencilFunc( func : Int, ref : Int, mask : Int ) : Void;
	function stencilFuncSeparate( face : Int, func : Int, ref : Int, mask : Int ) : Void;
	function stencilMask( mask : Int ) : Void;
	function stencilMaskSeparate( face : Int, mask : Int ) : Void;
	function stencilOp( fail : Int, zfail : Int, zpass : Int ) : Void;
	function stencilOpSeparate( face : Int, fail : Int, zfail : Int, zpass : Int ) : Void;
	function texParameterf( target : Int, pname : Int, param : Float ) : Void;
	function texParameteri( target : Int, pname : Int, param : Int ) : Void;
	function uniform1f( location : UniformLocation, x : Float ) : Void;
	function uniform2f( location : UniformLocation, x : Float, y : Float ) : Void;
	function uniform3f( location : UniformLocation, x : Float, y : Float, z : Float ) : Void;
	function uniform4f( location : UniformLocation, x : Float, y : Float, z : Float, w : Float ) : Void;
	function uniform1i( location : UniformLocation, x : Int ) : Void;
	function uniform2i( location : UniformLocation, x : Int, y : Int ) : Void;
	function uniform3i( location : UniformLocation, x : Int, y : Int, z : Int ) : Void;
	function uniform4i( location : UniformLocation, x : Int, y : Int, z : Int, w : Int ) : Void;
	function useProgram( program : Program ) : Void;
	function validateProgram( program : Program ) : Void;
	function vertexAttrib1f( indx : Int, x : Float ) : Void;
	@:overload( function( indx : Int, values : Array<Float>) : Void {} )
	function vertexAttrib1fv( indx : Int, values : js.lib.Float32Array ) : Void;
	function vertexAttrib2f( indx : Int, x : Float, y : Float ) : Void;
	@:overload( function( indx : Int, values : Array<Float>) : Void {} )
	function vertexAttrib2fv( indx : Int, values : js.lib.Float32Array ) : Void;
	function vertexAttrib3f( indx : Int, x : Float, y : Float, z : Float ) : Void;
	@:overload( function( indx : Int, values : Array<Float>) : Void {} )
	function vertexAttrib3fv( indx : Int, values : js.lib.Float32Array ) : Void;
	function vertexAttrib4f( indx : Int, x : Float, y : Float, z : Float, w : Float ) : Void;
	@:overload( function( indx : Int, values : Array<Float>) : Void {} )
	function vertexAttrib4fv( indx : Int, values : js.lib.Float32Array ) : Void;
	function vertexAttribPointer( indx : Int, size : Int, type : Int, normalized : Bool, stride : Int, offset : Int ) : Void;
	function viewport( x : Int, y : Int, width : Int, height : Int ) : Void;
}