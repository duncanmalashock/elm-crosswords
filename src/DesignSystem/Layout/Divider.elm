module DesignSystem.Layout.Divider exposing (Config, divider)

import Html
import Html.Styled exposing (..)
import Css exposing (..)
import Html.Styled.Attributes exposing (css)


type alias Config =
    {}


divider : List ( Config, List (Html msg) ) -> Html msg
divider sections =
    div
        [ css
            [ displayFlex
            , flexDirection row
            , justifyContent spaceBetween
            , alignItems center
            , height (pct 100)
            ]
        ]
        (List.map sectionView sections)


sectionView : ( Config, List (Html msg) ) -> Html msg
sectionView ( config, content ) =
    div
        [ css
            [ height (pct 100)
            , flexGrow (int 1)
            , flexShrink (int 1)
            , flexBasis (pct 100)
            ]
        ]
        content
