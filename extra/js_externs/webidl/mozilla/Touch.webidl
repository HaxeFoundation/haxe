/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dvcs.w3.org/hg/webevents/raw-file/default/touchevents.html
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Func="mozilla::dom::Touch::PrefEnabled"]
interface Touch {
  readonly    attribute long         identifier;
  readonly    attribute EventTarget? target;
  readonly    attribute long         screenX;
  readonly    attribute long         screenY;
  readonly    attribute long         clientX;
  readonly    attribute long         clientY;
  readonly    attribute long         pageX;
  readonly    attribute long         pageY;
  readonly    attribute long         radiusX;
  readonly    attribute long         radiusY;
  readonly    attribute float        rotationAngle;
  readonly    attribute float        force;
};
