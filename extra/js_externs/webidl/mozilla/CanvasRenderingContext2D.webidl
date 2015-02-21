/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

enum CanvasWindingRule { "nonzero", "evenodd" };

dictionary ContextAttributes2D {
  // whether or not we're planning to do a lot of readback operations
  boolean willReadFrequently = false;
  // signal if the canvas contains an alpha channel
  boolean alpha = true;
};

dictionary HitRegionOptions {
  DOMString id = "";
  Element? control = null;
};

interface CanvasRenderingContext2D {

  // back-reference to the canvas.  Might be null if we're not
  // associated with a canvas.
  readonly attribute HTMLCanvasElement? canvas;

  // state
  void save(); // push state on state stack
  void restore(); // pop state stack and restore state

  // transformations (default transform is the identity matrix)
// NOT IMPLEMENTED           attribute SVGMatrix currentTransform;
  [Throws, LenientFloat]
  void scale(double x, double y);
  [Throws, LenientFloat]
  void rotate(double angle);
  [Throws, LenientFloat]
  void translate(double x, double y);
  [Throws, LenientFloat]
  void transform(double a, double b, double c, double d, double e, double f);
  [Throws, LenientFloat]
  void setTransform(double a, double b, double c, double d, double e, double f);
  [Throws]
  void resetTransform();

  // compositing
           attribute unrestricted double globalAlpha; // (default 1.0)
           [Throws]
           attribute DOMString globalCompositeOperation; // (default source-over)

  // colors and styles (see also the CanvasDrawingStyles interface)
           attribute (DOMString or CanvasGradient or CanvasPattern) strokeStyle; // (default black)
           attribute (DOMString or CanvasGradient or CanvasPattern) fillStyle; // (default black)
  [NewObject]
  CanvasGradient createLinearGradient(double x0, double y0, double x1, double y1);
  [NewObject, Throws]
  CanvasGradient createRadialGradient(double x0, double y0, double r0, double x1, double y1, double r1);
  [NewObject, Throws]
  CanvasPattern createPattern((HTMLImageElement or HTMLCanvasElement or HTMLVideoElement) image, [TreatNullAs=EmptyString] DOMString repetition);

  // shadows
           [LenientFloat]
           attribute double shadowOffsetX; // (default 0)
           [LenientFloat]
           attribute double shadowOffsetY; // (default 0)
           [LenientFloat]
           attribute double shadowBlur; // (default 0)
           attribute DOMString shadowColor; // (default transparent black)

  [Pref="canvas.filters.enabled", SetterThrows]
  attribute DOMString filter; // (default empty string = no filter)

  // rects
  [LenientFloat]
  void clearRect(double x, double y, double w, double h);
  [LenientFloat]
  void fillRect(double x, double y, double w, double h);
  [LenientFloat]
  void strokeRect(double x, double y, double w, double h);

  // path API (see also CanvasPathMethods)
  void beginPath();
  void fill(optional CanvasWindingRule winding = "nonzero");
  void fill(Path2D path, optional CanvasWindingRule winding = "nonzero");
  void stroke();
  void stroke(Path2D path);
  [Pref="canvas.focusring.enabled"] void drawFocusIfNeeded(Element element);
// NOT IMPLEMENTED  void drawSystemFocusRing(Path path, HTMLElement element);
  [Pref="canvas.customfocusring.enabled"] boolean drawCustomFocusRing(Element element);
// NOT IMPLEMENTED  boolean drawCustomFocusRing(Path path, HTMLElement element);
// NOT IMPLEMENTED  void scrollPathIntoView();
// NOT IMPLEMENTED  void scrollPathIntoView(Path path);
  void clip(optional CanvasWindingRule winding = "nonzero");
  void clip(Path2D path, optional CanvasWindingRule winding = "nonzero");
// NOT IMPLEMENTED  void resetClip();
  boolean isPointInPath(unrestricted double x, unrestricted double y, optional CanvasWindingRule winding = "nonzero");
  boolean isPointInPath(Path2D path, unrestricted double x, unrestricted double y, optional CanvasWindingRule winding = "nonzero");
  boolean isPointInStroke(double x, double y);
  boolean isPointInStroke(Path2D path, unrestricted double x, unrestricted double y);

  // text (see also the CanvasDrawingStyles interface)
  [Throws, LenientFloat]
  void fillText(DOMString text, double x, double y, optional double maxWidth);
  [Throws, LenientFloat]
  void strokeText(DOMString text, double x, double y, optional double maxWidth);
  [NewObject, Throws]
  TextMetrics measureText(DOMString text);

  // drawing images
// NOT IMPLEMENTED           attribute boolean imageSmoothingEnabled; // (default true)
  [Throws, LenientFloat]
  void drawImage((HTMLImageElement or HTMLCanvasElement or HTMLVideoElement) image, double dx, double dy);
  [Throws, LenientFloat]
  void drawImage((HTMLImageElement or HTMLCanvasElement or HTMLVideoElement) image, double dx, double dy, double dw, double dh);
  [Throws, LenientFloat]
  void drawImage((HTMLImageElement or HTMLCanvasElement or HTMLVideoElement) image, double sx, double sy, double sw, double sh, double dx, double dy, double dw, double dh);

  // hit regions
  [Pref="canvas.hitregions.enabled", Throws] void addHitRegion(optional HitRegionOptions options);
  [Pref="canvas.hitregions.enabled"] void removeHitRegion(DOMString id);
  [Pref="canvas.hitregions.enabled"] void clearHitRegions();

  // pixel manipulation
  [NewObject, Throws]
  ImageData createImageData(double sw, double sh);
  [NewObject, Throws]
  ImageData createImageData(ImageData imagedata);
  [NewObject, Throws]
  ImageData getImageData(double sx, double sy, double sw, double sh);
  [Throws]
  void putImageData(ImageData imagedata, double dx, double dy);
  [Throws]
  void putImageData(ImageData imagedata, double dx, double dy, double dirtyX, double dirtyY, double dirtyWidth, double dirtyHeight);

  // Mozilla-specific stuff
  // FIXME Bug 768048 mozCurrentTransform/mozCurrentTransformInverse should return a WebIDL array.
  [Throws]
  attribute object mozCurrentTransform; // [ m11, m12, m21, m22, dx, dy ], i.e. row major
  [Throws]
  attribute object mozCurrentTransformInverse;

  attribute DOMString mozFillRule; /* "evenodd", "nonzero" (default) */

  [Throws]
  attribute any mozDash; /* default |null| */

  [LenientFloat]
  attribute double mozDashOffset; /* default 0.0 */

  [SetterThrows]
  attribute DOMString mozTextStyle;

  // image smoothing mode -- if disabled, images won't be smoothed
  // if scaled.
  attribute boolean mozImageSmoothingEnabled;

  // Show the caret if appropriate when drawing
  [ChromeOnly]
  const unsigned long DRAWWINDOW_DRAW_CARET   = 0x01;
  // Don't flush pending layout notifications that could otherwise
  // be batched up
  [ChromeOnly]
  const unsigned long DRAWWINDOW_DO_NOT_FLUSH = 0x02;
  // Draw scrollbars and scroll the viewport if they are present
  [ChromeOnly]
  const unsigned long DRAWWINDOW_DRAW_VIEW    = 0x04;
  // Use the widget layer manager if available. This means hardware
  // acceleration may be used, but it might actually be slower or
  // lower quality than normal. It will however more accurately reflect
  // the pixels rendered to the screen.
  [ChromeOnly]
  const unsigned long DRAWWINDOW_USE_WIDGET_LAYERS = 0x08;
  // Don't synchronously decode images - draw what we have
  [ChromeOnly]
  const unsigned long DRAWWINDOW_ASYNC_DECODE_IMAGES = 0x10;

  /**
   * Renders a region of a window into the canvas.  The contents of
   * the window's viewport are rendered, ignoring viewport clipping
   * and scrolling.
   *
   * @param x
   * @param y
   * @param w
   * @param h specify the area of the window to render, in CSS
   * pixels.
   *
   * @param backgroundColor the canvas is filled with this color
   * before we render the window into it. This color may be
   * transparent/translucent. It is given as a CSS color string
   * (e.g., rgb() or rgba()).
   *
   * @param flags Used to better control the drawWindow call.
   * Flags can be ORed together.
   *
   * Of course, the rendering obeys the current scale, transform and
   * globalAlpha values.
   *
   * Hints:
   * -- If 'rgba(0,0,0,0)' is used for the background color, the
   * drawing will be transparent wherever the window is transparent.
   * -- Top-level browsed documents are usually not transparent
   * because the user's background-color preference is applied,
   * but IFRAMEs are transparent if the page doesn't set a background.
   * -- If an opaque color is used for the background color, rendering
   * will be faster because we won't have to compute the window's
   * transparency.
   *
   * This API cannot currently be used by Web content. It is chrome
   * only.
   */
  [Throws, ChromeOnly]
  void drawWindow(Window window, double x, double y, double w, double h,
                  DOMString bgColor, optional unsigned long flags = 0);
  [Throws, ChromeOnly]
  void asyncDrawXULElement(XULElement elem, double x, double y, double w,
                           double h, DOMString bgColor,
                           optional unsigned long flags = 0);
  /**
   * This causes a context that is currently using a hardware-accelerated
   * backend to fallback to a software one. All state should be preserved.
   */
  [ChromeOnly]
  void demote();
};
CanvasRenderingContext2D implements CanvasDrawingStyles;
CanvasRenderingContext2D implements CanvasPathMethods;

[NoInterfaceObject]
interface CanvasDrawingStyles {
  // line caps/joins
           [LenientFloat]
           attribute double lineWidth; // (default 1)
           attribute DOMString lineCap; // "butt", "round", "square" (default "butt")
           [GetterThrows]
           attribute DOMString lineJoin; // "round", "bevel", "miter" (default "miter")
           [LenientFloat]
           attribute double miterLimit; // (default 10)

  // dashed lines
    [LenientFloat] void setLineDash(sequence<double> segments); // default empty
    sequence<double> getLineDash();
    [LenientFloat] attribute double lineDashOffset;

  // text
           [SetterThrows]
           attribute DOMString font; // (default 10px sans-serif)
           attribute DOMString textAlign; // "start", "end", "left", "right", "center" (default: "start")
           attribute DOMString textBaseline; // "top", "hanging", "middle", "alphabetic", "ideographic", "bottom" (default: "alphabetic")
};

[NoInterfaceObject]
interface CanvasPathMethods {
  // shared path API methods
  void closePath();
  [LenientFloat]
  void moveTo(double x, double y);
  [LenientFloat]
  void lineTo(double x, double y);
  [LenientFloat]
  void quadraticCurveTo(double cpx, double cpy, double x, double y);

  [LenientFloat]
  void bezierCurveTo(double cp1x, double cp1y, double cp2x, double cp2y, double x, double y);

  [Throws, LenientFloat]
  void arcTo(double x1, double y1, double x2, double y2, double radius); 
// NOT IMPLEMENTED  [LenientFloat] void arcTo(double x1, double y1, double x2, double y2, double radiusX, double radiusY, double rotation);

  [LenientFloat]
  void rect(double x, double y, double w, double h);

  [Throws, LenientFloat]
  void arc(double x, double y, double radius, double startAngle, double endAngle, optional boolean anticlockwise = false); 
// NOT IMPLEMENTED  [LenientFloat] void ellipse(double x, double y, double radiusX, double radiusY, double rotation, double startAngle, double endAngle, boolean anticlockwise);
};

interface CanvasGradient {
  // opaque object
  [Throws]
  // addColorStop should take a double
  void addColorStop(float offset, DOMString color);
};

interface CanvasPattern {
  // opaque object
  // [Throws, LenientFloat] - could not do this overload because of bug 1020975
  // void setTransform(double a, double b, double c, double d, double e, double f);

  // No throw necessary here - SVGMatrix is always good.
  void setTransform(SVGMatrix matrix);
};

interface TextMetrics {

  // x-direction
  readonly attribute double width; // advance width

  /*
   * NOT IMPLEMENTED YET

  readonly attribute double actualBoundingBoxLeft;
  readonly attribute double actualBoundingBoxRight;

  // y-direction
  readonly attribute double fontBoundingBoxAscent;
  readonly attribute double fontBoundingBoxDescent;
  readonly attribute double actualBoundingBoxAscent;
  readonly attribute double actualBoundingBoxDescent;
  readonly attribute double emHeightAscent;
  readonly attribute double emHeightDescent;
  readonly attribute double hangingBaseline;
  readonly attribute double alphabeticBaseline;
  readonly attribute double ideographicBaseline;
  */

};

[Pref="canvas.path.enabled",
 Constructor,
 Constructor(Path2D other),
 Constructor(DOMString pathString)]
interface Path2D
{
  void addPath(Path2D path, optional SVGMatrix transformation);
};
Path2D implements CanvasPathMethods;
