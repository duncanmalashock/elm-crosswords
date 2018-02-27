module Update exposing (..)


withDirection : { x | direction : v } -> v -> { x | direction : v }
withDirection record v =
    { record | direction = v }
