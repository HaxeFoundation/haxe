/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 */

dictionary ProfileTimelineLayerRect {
  long x = 0;
  long y = 0;
  long width = 0;
  long height = 0;
};

dictionary ProfileTimelineMarker {
  DOMString name = "";
  DOMHighResTimeStamp start = 0;
  DOMHighResTimeStamp end = 0;
  /* For ConsoleTime markers.  */
  DOMString causeName;
  /* For DOMEvent markers.  */
  DOMString type;
  unsigned short eventPhase;
  /* For Paint markers.  */
  sequence<ProfileTimelineLayerRect> rectangles;
};
