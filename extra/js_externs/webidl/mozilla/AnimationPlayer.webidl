/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dev.w3.org/fxtf/web-animations/#idl-def-AnimationPlayer
 *
 * Copyright © 2014 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

enum AnimationPlayState { "idle", "pending", "running", "paused", "finished" };

[Func="nsDocument::IsWebAnimationsEnabled"]
interface AnimationPlayer {
  // Bug 1049975
  //           attribute AnimationNode?     source;
  [Pure]
  readonly attribute Animation? source;
  readonly attribute AnimationTimeline timeline;
  [BinaryName="startTimeAsDouble"]
  readonly attribute double? startTime;
  [BinaryName="currentTimeAsDouble"]
  readonly attribute double? currentTime;

  /* Not yet implemented
           attribute double             playbackRate; */
  [BinaryName="playStateFromJS"]
  readonly attribute AnimationPlayState playState;
  /*
  readonly attribute Promise            ready;
  readonly attribute Promise            finished;
  void cancel ();
  void finish ();
  */
  [BinaryName="playFromJS"]
  void play ();
  [BinaryName="pauseFromJS"]
  void pause ();
  /*
  void reverse ();
  */
};

// Non-standard extensions
partial interface AnimationPlayer {
  [ChromeOnly] readonly attribute boolean isRunningOnCompositor;
};
