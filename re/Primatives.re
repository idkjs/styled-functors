open ReactNative;
open Style;
// open ReactNative.Platform;

[@bs.module "react-native-responsive-fontsize"]
external rf: float => float = "default";
type platform =
  | IOS
  | Android
  | Web;
let platform =
  if (Platform.os == Platform.ios) {
    IOS;
  } else if (Platform.os == Platform.web) {
    Web;
  } else {
    Android;
  };
let isWeb = () => {
  switch (platform) {
  | Android => false
  | IOS => false
  | Web => true
  };
};

module Box =
  Functors.BoxView({
    type paddingType = [ | `p(float) | `px(float) | `py(float)];
    type declaration = [ | `declaration(string, string)];
    type marginType = [
      | `mt(float)
      | `mb(float)
      | `mr(float)
      | `ml(float)
    ];
    let d = (property, value) => `declaration((property, value));
    let unsafe = d;
    let baseStyle = style() /*display(Flex)  hmmmm   */;
    let getPaddStyle = spaceType =>
      switch (spaceType) {
      | `p(p) => [unsafe("padding", pct(p))]
      | `px(p) => [
          unsafe("paddingRight", pct(p)),
          unsafe("paddingLeft", pct(p)),
        ]
      | `py(p) => [
          unsafe("paddingTop", pct(p)),
          unsafe("paddingBottom", pct(p)),
        ]
      };
    let getMargStyle = marginType =>
      switch (marginType) {
      | `mt(m) => unsafe("marginTop", pct(m))
      | `mb(m) => unsafe("marginBottom", pct(m))
      | `mr(m) => unsafe("marginRight", pct(m))
      | `ml(m) => unsafe("marginLeft", pct(m))
      };
    let paddingStyle = spaceType =>
      switch (spaceType) {
      | Some(spaceType) => getPaddStyle(spaceType)
      | None => baseStyle
      };
    let marginStyle = marginType =>
      switch (marginType) {
      | Some(marginType) => getMargStyle(marginType)
      | None => baseStyle
      };
    let bgColorStyle = bg =>
      switch (bg) {
      | Some(bg) => [unsafe("backgroundColor", bg)]
      | None => []
      };
    let style = (paddingType, marginType, bgColor) =>{
// open Style;
  StyleSheet.create({
    // style may be defined inline
    "marginStyle": style(~margin=marginStyle(marginType), ()),
    "paddingStyle": style(~padding=paddingStyle(paddingType), ()),
    // or already defined elsewhere
    "backgroundColore": bgColorStyle(bgColor)
  })
      style(
        List.append(
          List.flatten([
            marginStyle(marginType),
            paddingStyle(paddingType),
            bgColorStyle(bgColor),
          ]),
          baseStyle,
        ),
      )};
  });

module Flex =
  Functors.FlexView({
    type directionType = [ | `row | `column];
    type paddingType = [ | `p(float) | `px(float) | `py(float)];
    type marginType = [
      | `mt(float)
      | `mb(float)
      | `mr(float)
      | `ml(float)
    ];
    type flexType = [
      | `center
      | `centerX
      | `centerY
      | `right
      | `stretch
      | `around
    ];
    let baseStyle = [display(Flex)];
    let getFlexStyle = spaceType =>
      switch (spaceType) {
      | `center => [justifyContent(Center), alignItems(Center)]
      | `centerX => [alignItems(Center)]
      | `centerY => [justifyContent(Center)]
      | `right => [alignItems(FlexEnd)]
      | `stretch => [alignItems(Stretch)]
      | `around => [justifyContent(SpaceAround)]
      };
    let getPaddStyle = spaceType =>
      switch (spaceType) {
      | `p(p) => [padding(pct(p))]
      | `px(p) => [paddingRight(pct(p)), paddingLeft(pct(p))]
      | `py(p) => [paddingTop(pct(p)), paddingBottom(pct(p))]
      };
    let getMargStyle = marginType =>
      switch (marginType) {
      | `mt(m) => [marginTop(pct(m))]
      | `mb(m) => [marginBottom(pct(m))]
      | `mr(m) => [marginRight(pct(m))]
      | `ml(m) => [marginLeft(pct(m))]
      };
    let getDirStyle = directionType =>
      switch (directionType) {
      | `row => [flexDirection(Row)]
      | `column => []
      };
    let dirStyle = directionType =>
      switch (directionType) {
      | Some(directionType) => getDirStyle(directionType)
      | None => []
      };
    let paddingStyle = spaceType =>
      switch (spaceType) {
      | Some(spaceType) => getPaddStyle(spaceType)
      | None => []
      };
    let marginStyle = marginType =>
      switch (marginType) {
      | Some(marginType) => getMargStyle(marginType)
      | None => []
      };
    let flexStyle = flexType =>
      switch (flexType) {
      | Some(flexType) => getFlexStyle(flexType)
      | None => []
      };
    let growStyle = growType =>
      switch (growType) {
      | Some(_growType) => [flex(1.)]
      | None => []
      };
    let bgColorStyle = bg =>
      switch (bg) {
      | Some(bg) => [backgroundColor(String(bg))]
      | None => []
      };
    let style =
        (paddingType, marginType, flexType, directionType, grow, bgColor) =>
      style(
        List.append(
          List.flatten([
            marginStyle(marginType),
            paddingStyle(paddingType),
            flexStyle(flexType),
            dirStyle(directionType),
            growStyle(grow),
            bgColorStyle(bgColor),
          ]),
          baseStyle,
        ),
      );
  });

module T =
  Functors.Text({
    type fontType = [ | `size(float)];
    type paddingType = [ | `p(float) | `px(float) | `py(float)];
    type marginType = [
      | `mt(float)
      | `mb(float)
      | `mr(float)
      | `ml(float)
    ];
    let baseStyle = [] /*display(Flex)  hmmmm   */;
    let getPaddStyle = spaceType =>
      switch (spaceType) {
      | `p(p) => [padding(pct(p))]
      | `px(p) => [paddingRight(pct(p)), paddingLeft(pct(p))]
      | `py(p) => [paddingTop(pct(p)), paddingBottom(pct(p))]
      };
    let getMargStyle = marginType =>
      switch (marginType) {
      | `mt(m) => [marginTop(pct(m))]
      | `mb(m) => [marginBottom(pct(m))]
      | `mr(m) => [marginRight(pct(m))]
      | `ml(m) => [marginLeft(pct(m))]
      };
    let getFontStyle = fontType =>
      /* These hardcoded values say for (rn-) web, just default to an iPad height */
      /* Def not the best way to do this, but I'm tired. */
      switch (fontType) {
      | `size(s) => [fontSize(Float(isWeb() ? s *. 1024. /. 100. : rf(s)))]
      };
    let paddingStyle = spaceType =>
      switch (spaceType) {
      | Some(spaceType) => getPaddStyle(spaceType)
      | None => []
      };
    let marginStyle = marginType =>
      switch (marginType) {
      | Some(marginType) => getMargStyle(marginType)
      | None => []
      };
    let fontStyle = fontType =>
      switch (fontType) {
      | Some(fontType) => getFontStyle(fontType)
      | None => []
      };
    let colorStyle = color_ =>
      switch (color_) {
      | Some(color_) => [color(String(color_))]
      | None => []
      };
    let centerStyle = center =>
      switch (center) {
      | Some(_) => [textAlign(Center)]
      | None => []
      };
    let style = (paddingType, marginType, fontType, color, center) =>
      style(
        List.append(
          List.flatten([
            marginStyle(marginType),
            paddingStyle(paddingType),
            fontStyle(fontType),
            colorStyle(color),
            centerStyle(center),
          ]),
          baseStyle,
        ),
      );
  });
