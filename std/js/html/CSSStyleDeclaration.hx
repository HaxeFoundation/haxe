/*
 * Copyright (C)2005-2017 Haxe Foundation
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

// This file is generated from mozilla\CSSStyleDeclaration.webidl. Do not edit!

package js.html;

/**
	`CSSStyleDeclaration` represents a collection of CSS property-value pairs. It is used in a few APIs:

	Documentation [CSSStyleDeclaration](https://developer.mozilla.org/en-US/docs/Web/API/CSSStyleDeclaration) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CSSStyleDeclaration$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CSSStyleDeclaration>
**/
@:native("CSSStyleDeclaration")
extern class CSSStyleDeclaration implements ArrayAccess<String>
{
	var cssText : String;
	var length(default,null) : Int;
	var parentRule(default,null) : CSSRule;
	
	/** Shorthand for the "align-content" CSS property. */
	var alignContent :String;
	/** Shorthand for the "align-items" CSS property. */
	var alignItems :String;
	/** Shorthand for the "align-self" CSS property. */
	var alignSelf :String;
	/** Shorthand for the "alignment-adjust" CSS property. */
	var alignmentAdjust :String;
	/** Shorthand for the "alignment-baseline" CSS property. */
	var alignmentBaseline :String;
	/** Shorthand for the "all" CSS property. */
	var all :String;
	/** Shorthand for the "anchor-point" CSS property. */
	var anchorPoint :String;
	/** Shorthand for the "animation" CSS property. */
	var animation :String;
	/** Shorthand for the "animation-delay" CSS property. */
	var animationDelay :String;
	/** Shorthand for the "animation-direction" CSS property. */
	var animationDirection :String;
	/** Shorthand for the "animation-duration" CSS property. */
	var animationDuration :String;
	/** Shorthand for the "animation-fill-mode" CSS property. */
	var animationFillMode :String;
	/** Shorthand for the "animation-iteration-count" CSS property. */
	var animationIterationCount :String;
	/** Shorthand for the "animation-name" CSS property. */
	var animationName :String;
	/** Shorthand for the "animation-play-state" CSS property. */
	var animationPlayState :String;
	/** Shorthand for the "animation-timing-function" CSS property. */
	var animationTimingFunction :String;
	/** Shorthand for the "azimuth" CSS property. */
	var azimuth :String;
	/** Shorthand for the "backface-visibility" CSS property. */
	var backfaceVisibility :String;
	/** Shorthand for the "background" CSS property. */
	var background :String;
	/** Shorthand for the "background-attachment" CSS property. */
	var backgroundAttachment :String;
	/** Shorthand for the "background-clip" CSS property. */
	var backgroundClip :String;
	/** Shorthand for the "background-color" CSS property. */
	var backgroundColor :String;
	/** Shorthand for the "background-image" CSS property. */
	var backgroundImage :String;
	/** Shorthand for the "background-origin" CSS property. */
	var backgroundOrigin :String;
	/** Shorthand for the "background-position" CSS property. */
	var backgroundPosition :String;
	/** Shorthand for the "background-repeat" CSS property. */
	var backgroundRepeat :String;
	/** Shorthand for the "background-size" CSS property. */
	var backgroundSize :String;
	/** Shorthand for the "baseline-shift" CSS property. */
	var baselineShift :String;
	/** Shorthand for the "binding" CSS property. */
	var binding :String;
	/** Shorthand for the "bleed" CSS property. */
	var bleed :String;
	/** Shorthand for the "bookmark-label" CSS property. */
	var bookmarkLabel :String;
	/** Shorthand for the "bookmark-level" CSS property. */
	var bookmarkLevel :String;
	/** Shorthand for the "bookmark-state" CSS property. */
	var bookmarkState :String;
	/** Shorthand for the "border" CSS property. */
	var border :String;
	/** Shorthand for the "border-bottom" CSS property. */
	var borderBottom :String;
	/** Shorthand for the "border-bottom-color" CSS property. */
	var borderBottomColor :String;
	/** Shorthand for the "border-bottom-left-radius" CSS property. */
	var borderBottomLeftRadius :String;
	/** Shorthand for the "border-bottom-right-radius" CSS property. */
	var borderBottomRightRadius :String;
	/** Shorthand for the "border-bottom-style" CSS property. */
	var borderBottomStyle :String;
	/** Shorthand for the "border-bottom-width" CSS property. */
	var borderBottomWidth :String;
	/** Shorthand for the "border-collapse" CSS property. */
	var borderCollapse :String;
	/** Shorthand for the "border-color" CSS property. */
	var borderColor :String;
	/** Shorthand for the "border-image" CSS property. */
	var borderImage :String;
	/** Shorthand for the "border-image-outset" CSS property. */
	var borderImageOutset :String;
	/** Shorthand for the "border-image-repeat" CSS property. */
	var borderImageRepeat :String;
	/** Shorthand for the "border-image-slice" CSS property. */
	var borderImageSlice :String;
	/** Shorthand for the "border-image-source" CSS property. */
	var borderImageSource :String;
	/** Shorthand for the "border-image-width" CSS property. */
	var borderImageWidth :String;
	/** Shorthand for the "border-left" CSS property. */
	var borderLeft :String;
	/** Shorthand for the "border-left-color" CSS property. */
	var borderLeftColor :String;
	/** Shorthand for the "border-left-style" CSS property. */
	var borderLeftStyle :String;
	/** Shorthand for the "border-left-width" CSS property. */
	var borderLeftWidth :String;
	/** Shorthand for the "border-radius" CSS property. */
	var borderRadius :String;
	/** Shorthand for the "border-right" CSS property. */
	var borderRight :String;
	/** Shorthand for the "border-right-color" CSS property. */
	var borderRightColor :String;
	/** Shorthand for the "border-right-style" CSS property. */
	var borderRightStyle :String;
	/** Shorthand for the "border-right-width" CSS property. */
	var borderRightWidth :String;
	/** Shorthand for the "border-spacing" CSS property. */
	var borderSpacing :String;
	/** Shorthand for the "border-style" CSS property. */
	var borderStyle :String;
	/** Shorthand for the "border-top" CSS property. */
	var borderTop :String;
	/** Shorthand for the "border-top-color" CSS property. */
	var borderTopColor :String;
	/** Shorthand for the "border-top-left-radius" CSS property. */
	var borderTopLeftRadius :String;
	/** Shorthand for the "border-top-right-radius" CSS property. */
	var borderTopRightRadius :String;
	/** Shorthand for the "border-top-style" CSS property. */
	var borderTopStyle :String;
	/** Shorthand for the "border-top-width" CSS property. */
	var borderTopWidth :String;
	/** Shorthand for the "border-width" CSS property. */
	var borderWidth :String;
	/** Shorthand for the "bottom" CSS property. */
	var bottom :String;
	/** Shorthand for the "box-decoration-break" CSS property. */
	var boxDecorationBreak :String;
	/** Shorthand for the "box-shadow" CSS property. */
	var boxShadow :String;
	/** Shorthand for the "box-sizing" CSS property. */
	var boxSizing :String;
	/** Shorthand for the "box-snap" CSS property. */
	var boxSnap :String;
	/** Shorthand for the "box-suppress" CSS property. */
	var boxSuppress :String;
	/** Shorthand for the "break-after" CSS property. */
	var breakAfter :String;
	/** Shorthand for the "break-before" CSS property. */
	var breakBefore :String;
	/** Shorthand for the "break-inside" CSS property. */
	var breakInside :String;
	/** Shorthand for the "caption-side" CSS property. */
	var captionSide :String;
	/** Shorthand for the "chains" CSS property. */
	var chains :String;
	/** Shorthand for the "clear" CSS property. */
	var clear :String;
	/** Shorthand for the "clip" CSS property. */
	var clip :String;
	/** Shorthand for the "clip-path" CSS property. */
	var clipPath :String;
	/** Shorthand for the "clip-rule" CSS property. */
	var clipRule :String;
	/** Shorthand for the "color" CSS property. */
	var color :String;
	/** Shorthand for the "color-interpolation-filters" CSS property. */
	var colorInterpolationFilters :String;
	/** Shorthand for the "column-count" CSS property. */
	var columnCount :String;
	/** Shorthand for the "column-fill" CSS property. */
	var columnFill :String;
	/** Shorthand for the "column-gap" CSS property. */
	var columnGap :String;
	/** Shorthand for the "column-rule" CSS property. */
	var columnRule :String;
	/** Shorthand for the "column-rule-color" CSS property. */
	var columnRuleColor :String;
	/** Shorthand for the "column-rule-style" CSS property. */
	var columnRuleStyle :String;
	/** Shorthand for the "column-rule-width" CSS property. */
	var columnRuleWidth :String;
	/** Shorthand for the "column-span" CSS property. */
	var columnSpan :String;
	/** Shorthand for the "column-width" CSS property. */
	var columnWidth :String;
	/** Shorthand for the "columns" CSS property. */
	var columns :String;
	/** Shorthand for the "contain" CSS property. */
	var contain :String;
	/** Shorthand for the "content" CSS property. */
	var content :String;
	/** Shorthand for the "counter-increment" CSS property. */
	var counterIncrement :String;
	/** Shorthand for the "counter-reset" CSS property. */
	var counterReset :String;
	/** Shorthand for the "counter-set" CSS property. */
	var counterSet :String;
	/** Shorthand for the "crop" CSS property. */
	var crop :String;
	/** Shorthand for the "cue" CSS property. */
	var cue :String;
	/** Shorthand for the "cue-after" CSS property. */
	var cueAfter :String;
	/** Shorthand for the "cue-before" CSS property. */
	var cueBefore :String;
	/** Shorthand for the "cursor" CSS property. */
	var cursor :String;
	/** Shorthand for the "direction" CSS property. */
	var direction :String;
	/** Shorthand for the "display" CSS property. */
	var display :String;
	/** Shorthand for the "display-inside" CSS property. */
	var displayInside :String;
	/** Shorthand for the "display-list" CSS property. */
	var displayList :String;
	/** Shorthand for the "display-outside" CSS property. */
	var displayOutside :String;
	/** Shorthand for the "dominant-baseline" CSS property. */
	var dominantBaseline :String;
	/** Shorthand for the "elevation" CSS property. */
	var elevation :String;
	/** Shorthand for the "empty-cells" CSS property. */
	var emptyCells :String;
	/** Shorthand for the "filter" CSS property. */
	var filter :String;
	/** Shorthand for the "flex" CSS property. */
	var flex :String;
	/** Shorthand for the "flex-basis" CSS property. */
	var flexBasis :String;
	/** Shorthand for the "flex-direction" CSS property. */
	var flexDirection :String;
	/** Shorthand for the "flex-flow" CSS property. */
	var flexFlow :String;
	/** Shorthand for the "flex-grow" CSS property. */
	var flexGrow :String;
	/** Shorthand for the "flex-shrink" CSS property. */
	var flexShrink :String;
	/** Shorthand for the "flex-wrap" CSS property. */
	var flexWrap :String;
	/** Shorthand for the "float" CSS property. */
	var float :String;
	/** Shorthand for the "float-offset" CSS property. */
	var floatOffset :String;
	/** Shorthand for the "flood-color" CSS property. */
	var floodColor :String;
	/** Shorthand for the "flood-opacity" CSS property. */
	var floodOpacity :String;
	/** Shorthand for the "flow-from" CSS property. */
	var flowFrom :String;
	/** Shorthand for the "flow-into" CSS property. */
	var flowInto :String;
	/** Shorthand for the "font" CSS property. */
	var font :String;
	/** Shorthand for the "font-family" CSS property. */
	var fontFamily :String;
	/** Shorthand for the "font-feature-settings" CSS property. */
	var fontFeatureSettings :String;
	/** Shorthand for the "font-kerning" CSS property. */
	var fontKerning :String;
	/** Shorthand for the "font-language-override" CSS property. */
	var fontLanguageOverride :String;
	/** Shorthand for the "font-size" CSS property. */
	var fontSize :String;
	/** Shorthand for the "font-size-adjust" CSS property. */
	var fontSizeAdjust :String;
	/** Shorthand for the "font-stretch" CSS property. */
	var fontStretch :String;
	/** Shorthand for the "font-style" CSS property. */
	var fontStyle :String;
	/** Shorthand for the "font-synthesis" CSS property. */
	var fontSynthesis :String;
	/** Shorthand for the "font-variant" CSS property. */
	var fontVariant :String;
	/** Shorthand for the "font-variant-alternates" CSS property. */
	var fontVariantAlternates :String;
	/** Shorthand for the "font-variant-caps" CSS property. */
	var fontVariantCaps :String;
	/** Shorthand for the "font-variant-east-asian" CSS property. */
	var fontVariantEastAsian :String;
	/** Shorthand for the "font-variant-ligatures" CSS property. */
	var fontVariantLigatures :String;
	/** Shorthand for the "font-variant-numeric" CSS property. */
	var fontVariantNumeric :String;
	/** Shorthand for the "font-variant-position" CSS property. */
	var fontVariantPosition :String;
	/** Shorthand for the "font-weight" CSS property. */
	var fontWeight :String;
	/** Shorthand for the "grid" CSS property. */
	var grid :String;
	/** Shorthand for the "grid-area" CSS property. */
	var gridArea :String;
	/** Shorthand for the "grid-auto-columns" CSS property. */
	var gridAutoColumns :String;
	/** Shorthand for the "grid-auto-flow" CSS property. */
	var gridAutoFlow :String;
	/** Shorthand for the "grid-auto-rows" CSS property. */
	var gridAutoRows :String;
	/** Shorthand for the "grid-column" CSS property. */
	var gridColumn :String;
	/** Shorthand for the "grid-column-end" CSS property. */
	var gridColumnEnd :String;
	/** Shorthand for the "grid-column-start" CSS property. */
	var gridColumnStart :String;
	/** Shorthand for the "grid-row" CSS property. */
	var gridRow :String;
	/** Shorthand for the "grid-row-end" CSS property. */
	var gridRowEnd :String;
	/** Shorthand for the "grid-row-start" CSS property. */
	var gridRowStart :String;
	/** Shorthand for the "grid-template" CSS property. */
	var gridTemplate :String;
	/** Shorthand for the "grid-template-areas" CSS property. */
	var gridTemplateAreas :String;
	/** Shorthand for the "grid-template-columns" CSS property. */
	var gridTemplateColumns :String;
	/** Shorthand for the "grid-template-rows" CSS property. */
	var gridTemplateRows :String;
	/** Shorthand for the "hanging-punctuation" CSS property. */
	var hangingPunctuation :String;
	/** Shorthand for the "height" CSS property. */
	var height :String;
	/** Shorthand for the "hyphens" CSS property. */
	var hyphens :String;
	/** Shorthand for the "icon" CSS property. */
	var icon :String;
	/** Shorthand for the "image-orientation" CSS property. */
	var imageOrientation :String;
	/** Shorthand for the "image-resolution" CSS property. */
	var imageResolution :String;
	/** Shorthand for the "ime-mode" CSS property. */
	var imeMode :String;
	/** Shorthand for the "initial-letters" CSS property. */
	var initialLetters :String;
	/** Shorthand for the "inline-box-align" CSS property. */
	var inlineBoxAlign :String;
	/** Shorthand for the "justify-content" CSS property. */
	var justifyContent :String;
	/** Shorthand for the "justify-items" CSS property. */
	var justifyItems :String;
	/** Shorthand for the "justify-self" CSS property. */
	var justifySelf :String;
	/** Shorthand for the "left" CSS property. */
	var left :String;
	/** Shorthand for the "letter-spacing" CSS property. */
	var letterSpacing :String;
	/** Shorthand for the "lighting-color" CSS property. */
	var lightingColor :String;
	/** Shorthand for the "line-box-contain" CSS property. */
	var lineBoxContain :String;
	/** Shorthand for the "line-break" CSS property. */
	var lineBreak :String;
	/** Shorthand for the "line-grid" CSS property. */
	var lineGrid :String;
	/** Shorthand for the "line-height" CSS property. */
	var lineHeight :String;
	/** Shorthand for the "line-snap" CSS property. */
	var lineSnap :String;
	/** Shorthand for the "line-stacking" CSS property. */
	var lineStacking :String;
	/** Shorthand for the "line-stacking-ruby" CSS property. */
	var lineStackingRuby :String;
	/** Shorthand for the "line-stacking-shift" CSS property. */
	var lineStackingShift :String;
	/** Shorthand for the "line-stacking-strategy" CSS property. */
	var lineStackingStrategy :String;
	/** Shorthand for the "list-style" CSS property. */
	var listStyle :String;
	/** Shorthand for the "list-style-image" CSS property. */
	var listStyleImage :String;
	/** Shorthand for the "list-style-position" CSS property. */
	var listStylePosition :String;
	/** Shorthand for the "list-style-type" CSS property. */
	var listStyleType :String;
	/** Shorthand for the "margin" CSS property. */
	var margin :String;
	/** Shorthand for the "margin-bottom" CSS property. */
	var marginBottom :String;
	/** Shorthand for the "margin-left" CSS property. */
	var marginLeft :String;
	/** Shorthand for the "margin-right" CSS property. */
	var marginRight :String;
	/** Shorthand for the "margin-top" CSS property. */
	var marginTop :String;
	/** Shorthand for the "marker-offset" CSS property. */
	var markerOffset :String;
	/** Shorthand for the "marker-side" CSS property. */
	var markerSide :String;
	/** Shorthand for the "marks" CSS property. */
	var marks :String;
	/** Shorthand for the "mask" CSS property. */
	var mask :String;
	/** Shorthand for the "mask-box" CSS property. */
	var maskBox :String;
	/** Shorthand for the "mask-box-outset" CSS property. */
	var maskBoxOutset :String;
	/** Shorthand for the "mask-box-repeat" CSS property. */
	var maskBoxRepeat :String;
	/** Shorthand for the "mask-box-slice" CSS property. */
	var maskBoxSlice :String;
	/** Shorthand for the "mask-box-source" CSS property. */
	var maskBoxSource :String;
	/** Shorthand for the "mask-box-width" CSS property. */
	var maskBoxWidth :String;
	/** Shorthand for the "mask-clip" CSS property. */
	var maskClip :String;
	/** Shorthand for the "mask-image" CSS property. */
	var maskImage :String;
	/** Shorthand for the "mask-origin" CSS property. */
	var maskOrigin :String;
	/** Shorthand for the "mask-position" CSS property. */
	var maskPosition :String;
	/** Shorthand for the "mask-repeat" CSS property. */
	var maskRepeat :String;
	/** Shorthand for the "mask-size" CSS property. */
	var maskSize :String;
	/** Shorthand for the "mask-source-type" CSS property. */
	var maskSourceType :String;
	/** Shorthand for the "mask-type" CSS property. */
	var maskType :String;
	/** Shorthand for the "max-height" CSS property. */
	var maxHeight :String;
	/** Shorthand for the "max-lines" CSS property. */
	var maxLines :String;
	/** Shorthand for the "max-width" CSS property. */
	var maxWidth :String;
	/** Shorthand for the "min-height" CSS property. */
	var minHeight :String;
	/** Shorthand for the "min-width" CSS property. */
	var minWidth :String;
	/** Shorthand for the "move-to" CSS property. */
	var moveTo :String;
	/** Shorthand for the "nav-down" CSS property. */
	var navDown :String;
	/** Shorthand for the "nav-index" CSS property. */
	var navIndex :String;
	/** Shorthand for the "nav-left" CSS property. */
	var navLeft :String;
	/** Shorthand for the "nav-right" CSS property. */
	var navRight :String;
	/** Shorthand for the "nav-up" CSS property. */
	var navUp :String;
	/** Shorthand for the "object-fit" CSS property. */
	var objectFit :String;
	/** Shorthand for the "object-position" CSS property. */
	var objectPosition :String;
	/** Shorthand for the "opacity" CSS property. */
	var opacity :String;
	/** Shorthand for the "order" CSS property. */
	var order :String;
	/** Shorthand for the "orphans" CSS property. */
	var orphans :String;
	/** Shorthand for the "outline" CSS property. */
	var outline :String;
	/** Shorthand for the "outline-color" CSS property. */
	var outlineColor :String;
	/** Shorthand for the "outline-offset" CSS property. */
	var outlineOffset :String;
	/** Shorthand for the "outline-style" CSS property. */
	var outlineStyle :String;
	/** Shorthand for the "outline-width" CSS property. */
	var outlineWidth :String;
	/** Shorthand for the "overflow" CSS property. */
	var overflow :String;
	/** Shorthand for the "overflow-wrap" CSS property. */
	var overflowWrap :String;
	/** Shorthand for the "overflow-x" CSS property. */
	var overflowX :String;
	/** Shorthand for the "overflow-y" CSS property. */
	var overflowY :String;
	/** Shorthand for the "padding" CSS property. */
	var padding :String;
	/** Shorthand for the "padding-bottom" CSS property. */
	var paddingBottom :String;
	/** Shorthand for the "padding-left" CSS property. */
	var paddingLeft :String;
	/** Shorthand for the "padding-right" CSS property. */
	var paddingRight :String;
	/** Shorthand for the "padding-top" CSS property. */
	var paddingTop :String;
	/** Shorthand for the "page" CSS property. */
	var page :String;
	/** Shorthand for the "page-break-after" CSS property. */
	var pageBreakAfter :String;
	/** Shorthand for the "page-break-before" CSS property. */
	var pageBreakBefore :String;
	/** Shorthand for the "page-break-inside" CSS property. */
	var pageBreakInside :String;
	/** Shorthand for the "page-policy" CSS property. */
	var pagePolicy :String;
	/** Shorthand for the "pause" CSS property. */
	var pause :String;
	/** Shorthand for the "pause-after" CSS property. */
	var pauseAfter :String;
	/** Shorthand for the "pause-before" CSS property. */
	var pauseBefore :String;
	/** Shorthand for the "perspective" CSS property. */
	var perspective :String;
	/** Shorthand for the "perspective-origin" CSS property. */
	var perspectiveOrigin :String;
	/** Shorthand for the "pitch" CSS property. */
	var pitch :String;
	/** Shorthand for the "pitch-range" CSS property. */
	var pitchRange :String;
	/** Shorthand for the "play-during" CSS property. */
	var playDuring :String;
	/** Shorthand for the "position" CSS property. */
	var position :String;
	/** Shorthand for the "presentation-level" CSS property. */
	var presentationLevel :String;
	/** Shorthand for the "quotes" CSS property. */
	var quotes :String;
	/** Shorthand for the "region-fragment" CSS property. */
	var regionFragment :String;
	/** Shorthand for the "resize" CSS property. */
	var resize :String;
	/** Shorthand for the "rest" CSS property. */
	var rest :String;
	/** Shorthand for the "rest-after" CSS property. */
	var restAfter :String;
	/** Shorthand for the "rest-before" CSS property. */
	var restBefore :String;
	/** Shorthand for the "richness" CSS property. */
	var richness :String;
	/** Shorthand for the "right" CSS property. */
	var right :String;
	/** Shorthand for the "rotation" CSS property. */
	var rotation :String;
	/** Shorthand for the "rotation-point" CSS property. */
	var rotationPoint :String;
	/** Shorthand for the "ruby-align" CSS property. */
	var rubyAlign :String;
	/** Shorthand for the "ruby-merge" CSS property. */
	var rubyMerge :String;
	/** Shorthand for the "ruby-position" CSS property. */
	var rubyPosition :String;
	/** Shorthand for the "shape-image-threshold" CSS property. */
	var shapeImageThreshold :String;
	/** Shorthand for the "shape-outside" CSS property. */
	var shapeOutside :String;
	/** Shorthand for the "shape-margin" CSS property. */
	var shapeMargin :String;
	/** Shorthand for the "size" CSS property. */
	var size :String;
	/** Shorthand for the "speak" CSS property. */
	var speak :String;
	/** Shorthand for the "speak-as" CSS property. */
	var speakAs :String;
	/** Shorthand for the "speak-header" CSS property. */
	var speakHeader :String;
	/** Shorthand for the "speak-numeral" CSS property. */
	var speakNumeral :String;
	/** Shorthand for the "speak-punctuation" CSS property. */
	var speakPunctuation :String;
	/** Shorthand for the "speech-rate" CSS property. */
	var speechRate :String;
	/** Shorthand for the "stress" CSS property. */
	var stress :String;
	/** Shorthand for the "string-set" CSS property. */
	var stringSet :String;
	/** Shorthand for the "tab-size" CSS property. */
	var tabSize :String;
	/** Shorthand for the "table-layout" CSS property. */
	var tableLayout :String;
	/** Shorthand for the "text-align" CSS property. */
	var textAlign :String;
	/** Shorthand for the "text-align-last" CSS property. */
	var textAlignLast :String;
	/** Shorthand for the "text-combine-upright" CSS property. */
	var textCombineUpright :String;
	/** Shorthand for the "text-decoration" CSS property. */
	var textDecoration :String;
	/** Shorthand for the "text-decoration-color" CSS property. */
	var textDecorationColor :String;
	/** Shorthand for the "text-decoration-line" CSS property. */
	var textDecorationLine :String;
	/** Shorthand for the "text-decoration-skip" CSS property. */
	var textDecorationSkip :String;
	/** Shorthand for the "text-decoration-style" CSS property. */
	var textDecorationStyle :String;
	/** Shorthand for the "text-emphasis" CSS property. */
	var textEmphasis :String;
	/** Shorthand for the "text-emphasis-color" CSS property. */
	var textEmphasisColor :String;
	/** Shorthand for the "text-emphasis-position" CSS property. */
	var textEmphasisPosition :String;
	/** Shorthand for the "text-emphasis-style" CSS property. */
	var textEmphasisStyle :String;
	/** Shorthand for the "text-height" CSS property. */
	var textHeight :String;
	/** Shorthand for the "text-indent" CSS property. */
	var textIndent :String;
	/** Shorthand for the "text-justify" CSS property. */
	var textJustify :String;
	/** Shorthand for the "text-orientation" CSS property. */
	var textOrientation :String;
	/** Shorthand for the "text-overflow" CSS property. */
	var textOverflow :String;
	/** Shorthand for the "text-shadow" CSS property. */
	var textShadow :String;
	/** Shorthand for the "text-space-collapse" CSS property. */
	var textSpaceCollapse :String;
	/** Shorthand for the "text-transform" CSS property. */
	var textTransform :String;
	/** Shorthand for the "text-underline-position" CSS property. */
	var textUnderlinePosition :String;
	/** Shorthand for the "text-wrap" CSS property. */
	var textWrap :String;
	/** Shorthand for the "top" CSS property. */
	var top :String;
	/** Shorthand for the "transform" CSS property. */
	var transform :String;
	/** Shorthand for the "transform-origin" CSS property. */
	var transformOrigin :String;
	/** Shorthand for the "transform-style" CSS property. */
	var transformStyle :String;
	/** Shorthand for the "transition" CSS property. */
	var transition :String;
	/** Shorthand for the "transition-delay" CSS property. */
	var transitionDelay :String;
	/** Shorthand for the "transition-duration" CSS property. */
	var transitionDuration :String;
	/** Shorthand for the "transition-property" CSS property. */
	var transitionProperty :String;
	/** Shorthand for the "transition-timing-function" CSS property. */
	var transitionTimingFunction :String;
	/** Shorthand for the "unicode-bidi" CSS property. */
	var unicodeBidi :String;
	/** Shorthand for the "vertical-align" CSS property. */
	var verticalAlign :String;
	/** Shorthand for the "visibility" CSS property. */
	var visibility :String;
	/** Shorthand for the "voice-balance" CSS property. */
	var voiceBalance :String;
	/** Shorthand for the "voice-duration" CSS property. */
	var voiceDuration :String;
	/** Shorthand for the "voice-family" CSS property. */
	var voiceFamily :String;
	/** Shorthand for the "voice-pitch" CSS property. */
	var voicePitch :String;
	/** Shorthand for the "voice-range" CSS property. */
	var voiceRange :String;
	/** Shorthand for the "voice-rate" CSS property. */
	var voiceRate :String;
	/** Shorthand for the "voice-stress" CSS property. */
	var voiceStress :String;
	/** Shorthand for the "voice-volume" CSS property. */
	var voiceVolume :String;
	/** Shorthand for the "volume" CSS property. */
	var volume :String;
	/** Shorthand for the "white-space" CSS property. */
	var whiteSpace :String;
	/** Shorthand for the "widows" CSS property. */
	var widows :String;
	/** Shorthand for the "width" CSS property. */
	var width :String;
	/** Shorthand for the "will-change" CSS property. */
	var willChange :String;
	/** Shorthand for the "word-break" CSS property. */
	var wordBreak :String;
	/** Shorthand for the "word-spacing" CSS property. */
	var wordSpacing :String;
	/** Shorthand for the "word-wrap" CSS property. */
	var wordWrap :String;
	/** Shorthand for the "wrap-flow" CSS property. */
	var wrapFlow :String;
	/** Shorthand for the "wrap-through" CSS property. */
	var wrapThrough :String;
	/** Shorthand for the "writing-mode" CSS property. */
	var writingMode :String;
	/** Shorthand for the "z-index" CSS property. */
	var zIndex :String;
	
	function item( index : Int ) : String;
	/** @throws DOMError */
	function getPropertyValue( property : String ) : String;
	/** @throws DOMError */
	function getPropertyCSSValue( property : String ) : CSSValue;
	function getPropertyPriority( property : String ) : String;
	/** @throws DOMError */
	function setProperty( property : String, value : String, ?priority : String = "" ) : Void;
	/** @throws DOMError */
	function removeProperty( property : String ) : String;
}