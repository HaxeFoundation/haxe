/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

interface SVGElement : Element {
           attribute DOMString id;
/*           [SetterThrows]
           attribute DOMString xmlbase; */

  readonly attribute SVGAnimatedString className;
  [PutForwards=cssText, Constant]
  readonly attribute CSSStyleDeclaration style;

  /*[SetterThrows]
  attribute DOMString xmllang;
  [SetterThrows]
  attribute DOMString xmlspace;*/

  readonly attribute SVGSVGElement? ownerSVGElement;
  readonly attribute SVGElement? viewportElement;

           attribute EventHandler oncopy;
           attribute EventHandler oncut;
           attribute EventHandler onpaste;
};

SVGElement implements GlobalEventHandlers;
SVGElement implements TouchEventHandlers;
SVGElement implements OnErrorEventHandlerForNodes;
