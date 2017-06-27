module Styles.Styles exposing (..)

import Css
    exposing
        ( Stylesheet
        , alignItems
        , auto
        , backgroundColor
        , bold
        , border3
        , borderBottom3
        , borderCollapse
        , borderRadius
        , boxShadow4
        , center
        , children
        , class
        , collapse
        , color
        , cursor
        , descendants
        , display
        , displayFlex
        , em
        , everything
        , flex2
        , flexEnd
        , flexFlow2
        , flexStart
        , fontFamily
        , fontSize
        , fontWeight
        , hover
        , justifyContent
        , left
        , listStyle
        , margin
        , margin2
        , margin4
        , marginBottom
        , maxWidth
        , noWrap
        , none
        , num
        , padding
        , padding2
        , padding4
        , pct
        , pointer
        , property
        , px
        , row
        , sansSerif
        , solid
        , spaceBetween
        , stretch
        , stylesheet
        , textAlign
        , textShadow4
        , withClass
        , wrap
        , zero
        )
import Css.Namespace
import Css.Elements exposing (body, h1, h2)
import Styles.Colors as Color
import Styles.Classes as Class


namespace : String
namespace =
    "roastery"


css : Stylesheet
css =
    (stylesheet << Css.Namespace.namespace namespace)
        [ everything
            [ margin zero
            , padding zero
            , fontFamily sansSerif
            ]
        , body
            [ backgroundColor Color.palette4
            ]
          -- , class Class.Wrapper
          --     [ displayFlex
          --     , maxWidth (px 1200)
          --     , margin4 (em 2) auto zero auto
          --     , flexFlow2 row wrap
          --     , borderRadius (em 0.5)
          --     , color Color.white
          --     , backgroundColor Color.palette1
          --     ]
          -- , class Class.Wrapper
          --     [ children
          --         [ everything
          --             [ flex2 (num 1) (pct 100) ]
          --         ]
          --     ]
        , class Class.Header
            [ backgroundColor Color.palette1
            , boxShadow4 zero (Css.rem 0.5) (Css.rem 1) Color.palette3
            ]
        , class Class.HeaderTitle
            [ margin (Css.rem 1)
            , color Color.palette3
            ]
        , class Class.UserWidget
            [ padding (em 1)
            , backgroundColor Color.palette3
            , color Color.palette1
            , displayFlex
            , flexFlow2 row noWrap
            , alignItems center
            , cursor pointer
            ]
        , class Class.Main
            [ property "flex" "1 0px"
            , padding2 zero (em 1)
            ]
        , class Class.Main
            [ descendants
                [ h2
                    [ fontSize (em 1.5)
                    ]
                ]
            ]
        , class Class.MenuList
            [ listStyle none
            , displayFlex
            , flexFlow2 row noWrap
            , justifyContent flexStart
            , alignItems flexStart
            ]
        , class Class.MenuItem
            [ margin4 (Css.rem 1) zero zero zero
            , padding2 (Css.rem 0.5) (Css.rem 1)
            , cursor pointer
            , hover
                [ textShadow4 zero zero (Css.rem 0.7) Color.palette3
                ]
            , withClass Class.ModSelected
                [ borderBottom3 (Css.rem 0.2) solid Color.palette1
                ]
            ]
        , class Class.Page
            [ padding (Css.rem 1) ]
        , class Class.PageTitle
            [ margin2 (Css.rem 2) zero
            , fontSize (Css.rem 3)
            ]
        , class Class.Wrapper
            [ maxWidth (Css.rem 60)
            , margin2 zero auto
            ]
        , class Class.HeaderWrapper
            [ displayFlex
            , justifyContent spaceBetween
            , alignItems stretch
            ]
        , class Class.Table
            [ border3 (Css.rem 0.1) solid Color.palette2
            , borderCollapse collapse
            ]
        , class Class.TableHeader
            [ backgroundColor Color.palette2
            ]
        , class Class.TableTh
            [ fontWeight bold
            , padding4 (Css.rem 0.5) (Css.rem 2) (Css.rem 0.5) (Css.rem 0.5)
            , textAlign left
            ]
        , class Class.TableTd
            [ padding4 (Css.rem 0.5) (Css.rem 2) (Css.rem 0.5) (Css.rem 0.5)
            , textAlign left
            ]
        , class Class.TableRow
            [ hover
                [ backgroundColor Color.palette3
                , cursor pointer
                ]
            ]
        ]
