module Styles.Styles exposing (..)

import Css
    exposing
        ( Stylesheet
        , stylesheet
        , children
        , everything
        , padding
        , margin
        , margin4
        , marginBottom
        , backgroundColor
        , borderRadius
        , zero
        , none
        , class
        , color
        , fontFamily
        , fontSize
        , sansSerif
        , display
        , displayFlex
        , property
        , flex2
        , flexFlow2
        , maxWidth
        , px
        , pct
        , num
        , rem
        , auto
        , row
        , wrap
        , noWrap
        , justifyContent
        , spaceBetween
        , listStyle
        )
import Css.Namespace
import Styles.Colors as Color
import Styles.Classes as Class


namespace : String
namespace =
    "roastery"


css : Stylesheet
css =
    (stylesheet << Css.Namespace.namespace "dreamwriter")
        [ everything
            [ margin zero
            , padding zero
            , fontFamily sansSerif
            ]
        , class Class.Wrapper
            [ displayFlex
            , maxWidth (px 1200)
            , margin4 (Css.rem 2) auto zero auto
            , flexFlow2 row wrap
            ]
        , class Class.Wrapper
            [ children
                [ everything
                    [ flex2 (num 1) (pct 100) ]
                ]
            ]
        , class Class.Header
            [ displayFlex
            , flexFlow2 row noWrap
            , justifyContent spaceBetween
            , marginBottom (Css.rem 2)
            ]
        , class Class.HeaderTitle
            [ fontSize (Css.rem 2)
            ]
        , class Class.UserWidget
            [ padding (Css.rem 0.5)
            , backgroundColor Color.userWidget
            , borderRadius (Css.rem 0.3)
            ]
        , class Class.Main
            [ flex2 (num 5) zero
            ]
        , class Class.LeftMenu
            [ property "flex" "1 auto"
              -- flex2 (num 1) auto
            ]
        , class Class.LeftMenuList
            [ listStyle none
            ]
        ]
